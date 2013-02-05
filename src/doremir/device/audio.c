
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/device/audio.h>

#include <doremir/atomic.h> // TODO improving
#include <doremir/thread.h>
#include <doremir/util.h>

#include <portaudio.h>

typedef doremir_device_audio_t                  device_t;
typedef doremir_device_audio_stream_t           stream_t;
typedef doremir_device_audio_session_t          session_t;
typedef doremir_device_audio_stream_callback_t  stream_callback_t;
typedef doremir_device_audio_session_callback_t session_callback_t;
typedef doremir_device_audio_status_callback_t  status_callback_t;

typedef PaDeviceIndex native_index_t;
typedef PaStream     *native_stream_t;

struct _doremir_device_audio_session_t {

    impl_t              impl;               // Dispatcher
    system_time_t       acquired;           // Time of acquisition (not used at the moment)

    list_t              devices;            // Cached device list

    device_t            def_input;          // Default devices, both possibly null
    device_t            def_output;         // If present, these are also in the above list
};

struct _doremir_device_audio_t {

    impl_t              impl;               // Dispatcher
    native_index_t      index;              // Native device index

    string_t            name;               // Cached names
    string_t            host_name;

    bool                muted;              // Not used at the moment
    double              volume;
};

struct _doremir_device_audio_stream_t {

    impl_t              impl;               // Dispatcher
    native_stream_t     native;             // Native stream

    device_t            input, output;
    processor_t         proc;
    proc_interface_t   *proc_impl;

    unsigned            input_channels, output_channels;
    int32_t             time;               // Monotonically increasing sample count

};

static mutex_t pa_mutex;
static bool    pa_status;

error_t audio_device_error(string_t msg);
error_t audio_device_error_with(string_t msg, int error);
ptr_t audio_session_impl(doremir_id_t interface);
ptr_t audio_device_impl(doremir_id_t interface);
ptr_t audio_stream_impl(doremir_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(native_index_t index);
inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t input, device_t output, processor_t proc);
inline static void delete_stream(stream_t stream);

void before_processing(stream_t stream);
void after_processing(stream_t stream);
int during_processing(stream_t stream, unsigned count, float **input, float **output);

static inline int num_input_channels(device_t device);
static inline int num_output_channels(device_t device);

static int native_audio_callback(const void *input_ptr,
                                 void *output_ptr,
                                 unsigned long frame_count,
                                 const PaStreamCallbackTimeInfo *time_info,
                                 PaStreamCallbackFlags flags,
                                 void *data);

static void native_finished_callback(void *data);

// --------------------------------------------------------------------------------

inline static session_t new_session()
{
    session_t session = doremir_new(device_audio_session);
    session->impl = &audio_session_impl;
    return session;
}

inline static void session_init_devices(session_t session)
{
    native_index_t count;
    list_t         devices;

    count   = Pa_GetDeviceCount();
    devices = doremir_list_empty();

    for (size_t i = 0; i < count; ++i) {
        device_t device = new_device(i);

        if (device) {
            devices = doremir_list_dcons(device, devices);
        }
    }

    session->devices      = doremir_list_dreverse(devices);
    session->def_input    = new_device(Pa_GetDefaultInputDevice());
    session->def_output   = new_device(Pa_GetDefaultOutputDevice());
}

inline static void delete_session(session_t session)
{
    // TODO free device list
    doremir_delete(session);
}

inline static device_t new_device(native_index_t index)
{
    if (index == paNoDevice) {
        return NULL;
    }

    device_t device = doremir_new(device_audio);
    device->impl    = &audio_device_impl;

    const PaDeviceInfo  *info      = Pa_GetDeviceInfo(index);
    const PaHostApiInfo *host_info = Pa_GetHostApiInfo(info->hostApi);

    device->index       = index;
    device->name        = string((char *) info->name);      // const cast
    device->host_name   = string((char *) host_info->name);
    device->muted       = false;
    device->volume      = 1.0;

    return device;
}

inline static void delete_device(device_t device)
{
    doremir_delete(device);
}

inline static stream_t new_stream(device_t input, device_t output, processor_t proc)
{
    stream_t stream         = doremir_new(device_audio_stream);

    stream->impl            = &audio_stream_impl;
    stream->input           = input;
    stream->output          = output;
    stream->input_channels  = num_input_channels(input);
    stream->output_channels = num_output_channels(output);

    stream->proc            = proc;
    stream->proc_impl       = doremir_interface(doremir_processor_interface_i, stream->proc);

    assert(stream->proc_impl && "Must implement Processor");

    return stream;
}

inline static void delete_stream(stream_t stream)
{
    doremir_delete(stream);
}


// --------------------------------------------------------------------------------

void doremir_device_audio_initialize()
{
    pa_mutex  = doremir_thread_create_mutex();
    pa_status = false;
}

void doremir_device_audio_terminate()
{
    doremir_thread_destroy_mutex(pa_mutex);
}

// --------------------------------------------------------------------------------

session_t doremir_device_audio_begin_session()
{
    if (!pa_mutex) {
        assert(false && "Not initalized");
    }

    inform(string("Initializing real-time audio session"));

    doremir_thread_lock(pa_mutex);
    {
        if (pa_status) {
            doremir_thread_unlock(pa_mutex);
            return (session_t) audio_device_error(string("Overlapping real-time audio sessions"));
        } else {
            Pa_Initialize();
            pa_status = true;
            doremir_thread_unlock(pa_mutex);

            session_t session = new_session();
            session_init_devices(session);
            // session->acquired = time(NULL);      // TODO
            return session;
        }
    }
}

void doremir_device_audio_end_session(session_t session)
{
    if (!pa_mutex) {
        assert(false && "Not initalized");
    }

    inform(string("Terminating real-time audio session"));

    doremir_thread_lock(pa_mutex);
    {
        if (pa_status) {
            Pa_Terminate();
            pa_status = false;
        }
    }
    doremir_thread_unlock(pa_mutex);
    delete_session(session);
}

void doremir_device_audio_with_session(
    session_callback_t session_callback,
    ptr_t                           session_data,
    error_callback_t                error_callback,
    ptr_t                           error_data
)
{
    session_t session = doremir_device_audio_begin_session();

    if (doremir_check(session)) {
        error_callback(error_data, (error_t) session);
    } else {
        session_callback(session_data, session);
    }

    doremir_device_audio_end_session(session);
}

doremir_list_t doremir_device_audio_all(session_t session)
{
    return doremir_copy(session->devices);
}

doremir_pair_t doremir_device_audio_default(session_t session)
{
    return pair(session->def_input, session->def_output);
}

device_t doremir_device_audio_default_input(session_t session)
{
    return session->def_input;
}

device_t doremir_device_audio_default_output(session_t session)
{
    return session->def_output;
}

doremir_string_t doremir_device_audio_name(device_t device)
{
    return doremir_copy(device->name);
}

doremir_string_t doremir_device_audio_host_name(device_t device)
{
    return doremir_copy(device->host_name);
}

type_t doremir_device_audio_input_type(device_t device)
{
    const PaDeviceInfo *info = Pa_GetDeviceInfo(device->index);
    return doremir_type_repeat(info->maxInputChannels, type_frame(type(f32)));
}

type_t doremir_device_audio_output_type(device_t device)
{
    const PaDeviceInfo  *info = Pa_GetDeviceInfo(device->index);
    return doremir_type_repeat(info->maxOutputChannels, type_frame(type(f32)));
}


void set_device_status_impl(status_callback_t function, ptr_t data);

void doremir_device_audio_set_status_callback(
    status_callback_t function,
    ptr_t             data,
    session_t         session)
{
    assert(session && "Not a real session");

    // See device_osx.c and device_win.c
    set_device_status_impl(function, data);
}

// --------------------------------------------------------------------------------

static inline int num_input_channels(device_t device)
{
    return device ? doremir_type_channels(doremir_device_audio_input_type(device)) : 0;
}
static inline int num_output_channels(device_t device)
{
    return device ? doremir_type_channels(doremir_device_audio_output_type(device)) : 0;
}

// TODO change sample rate
stream_t doremir_device_audio_open_stream(device_t input, processor_t proc, device_t output)
{
    PaError status;
    stream_t stream = new_stream(input, output, proc);

    inform(string("Opening real-time audio stream"));
    inform(string_dappend(string("    Input:  "), input ? doremir_string_show(input) : string("-")));
    inform(string_dappend(string("    Output: "), output ? doremir_string_show(output) : string("-")));

    {
        PaStreamParameters inp = {
            .device                     = input ? input->index : 0,
            .channelCount               = stream->input_channels,
            .sampleFormat               = (paFloat32 | paNonInterleaved),
            .suggestedLatency           = 0,
            .hostApiSpecificStreamInfo  = 0
        };

        PaStreamParameters outp = {
            .device                     = output ? output->index : 0,
            .channelCount               = stream->output_channels,
            .sampleFormat               = (paFloat32 | paNonInterleaved),
            .suggestedLatency           = 0,
            .hostApiSpecificStreamInfo  = 0
        };
        // printf("> %d\n", inp.channelCount);
        // printf("> %d\n", outp.channelCount);

        const PaStreamParameters       *in      = input ? &inp : NULL;
        const PaStreamParameters       *out     = output ? &outp : NULL;
        double                          sr      = 44100;
        unsigned long                   vs      = paFramesPerBufferUnspecified;
        PaStreamFlags                   flags   = paNoFlag;
        PaStreamCallback               *cb      = native_audio_callback;
        ptr_t                           data    = stream;

        status = Pa_OpenStream(&stream->native, in, out, sr, vs, flags, cb, data);

        if (status != paNoError) {
            return (stream_t) audio_device_error_with(string("Could not start stream"), status);
        }

        status = Pa_SetStreamFinishedCallback(stream->native, native_finished_callback);

        if (status != paNoError) {
            return (stream_t) audio_device_error_with(string("Could not start stream"), status);
        }
    }
    before_processing(stream);

    status = Pa_StartStream(stream->native);

    if (status != paNoError) {
        return (stream_t) audio_device_error_with(string("Could not start stream"), status);
    }

    return stream;
}

void doremir_device_audio_close_stream(stream_t stream)
{
    inform(string("Closing real-time audio stream"));

    Pa_CloseStream(stream->native);
    // after_processing() will be called from native_finished_callback
}

void doremir_device_audio_with_stream(device_t            input,
                                      processor_t         processor,
                                      device_t            output,
                                      stream_callback_t   stream_callback,
                                      ptr_t               stream_data,
                                      error_callback_t    error_callback,
                                      ptr_t               error_data)
{
    stream_t stream = doremir_device_audio_open_stream(input, processor, output);

    if (doremir_check(stream)) {
        error_callback(error_data, (error_t) stream);
    } else {
        stream_callback(stream_data, stream);
    }

    doremir_device_audio_close_stream(stream);
}


// --------------------------------------------------------------------------------

void before_processing(stream_t stream)
{
    // extract top-level audio type

    // allocate buffers
    // call setup() on top processor (passing outgoing message receiver)
    doremir_processor_info_t info = {
        .sample_rate = 44100,
        .frame_size  = 0,
        .sample_time = 0,
        .total_time  = 0,
        .dispatcher  = NULL
    };

    stream->proc_impl->before(stream->proc, &info);
}

void after_processing(stream_t stream)
{
    doremir_processor_info_t info = {
        .sample_rate = 44100,
        .frame_size  = 0,
        .sample_time = 0,
        .total_time  = 0,
        .dispatcher  = NULL
    };

    stream->proc_impl->after(stream->proc, &info);

    // unregister processors from incoming message dispatcher
    // free buffers
}

int during_processing(stream_t stream, unsigned count, float **input, float **output)
{
    doremir_processor_info_t info = {
        .sample_rate = 44100,
        .frame_size  = 0,
        .sample_time = 0,
        .total_time  = 0,
        .dispatcher  = NULL
    };

    // call dispatch() on the innner dispatcher

    // deliver inputs
    // stream->proc_impl->process(stream->proc, &info, NULL);
    // deliver outputs
    for (unsigned channel = 0; channel < stream->input_channels; ++channel) {
        float *in  = input[channel];
        float *out = output[channel];

        for (int i = 0; i < count; ++i) {
            // TODO
            out[i] = in[i];
        }
    }

    stream->time += count; // TODO atomic incr

    return paContinue;
}


/* The callbacks */

int native_audio_callback(const void                       *input,
                          void                             *output,
                          unsigned long                     count,
                          const PaStreamCallbackTimeInfo   *time_info,
                          PaStreamCallbackFlags             flags,
                          void                             *data)
{
    // TODO handle status flags?
    return during_processing(data, count, (float **) input, (float **) output);
}

void native_finished_callback(void *data)
{
    after_processing(data);
}


// --------------------------------------------------------------------------------

doremir_string_t audio_session_show(ptr_t a)
{
    string_t str = string("<AudioSession ");
    str = string_dappend(str, doremir_string_format_integer(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void audio_session_destroy(ptr_t a)
{
    doremir_device_audio_end_session(a);
}

ptr_t audio_session_impl(doremir_id_t interface)
{
    static doremir_string_show_t audio_session_show_impl
        = { audio_session_show };
    static doremir_destroy_t audio_session_destroy_impl
        = { audio_session_destroy };

    switch (interface) {
    case doremir_string_show_i:
        return &audio_session_show_impl;

    case doremir_destroy_i:
        return &audio_session_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

doremir_string_t audio_device_show(ptr_t a)
{
    device_t device = (device_t) a;

    string_t str = string("<AudioDevice ");
    str = string_dappend(str, doremir_device_audio_host_name(device));
    str = string_dappend(str, string(" "));
    str = string_dappend(str, doremir_device_audio_name(device));
    str = string_dappend(str, string(">"));
    return str;
}

ptr_t audio_device_impl(doremir_id_t interface)
{
    static doremir_string_show_t audio_device_show_impl
        = { audio_device_show };

    switch (interface) {
    case doremir_string_show_i:
        return &audio_device_show_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

doremir_string_t audio_stream_show(ptr_t a)
{
    string_t str = string("<AudioStream ");
    str = string_dappend(str, doremir_string_format_integer(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void audio_stream_destroy(ptr_t a)
{
    doremir_device_audio_close_stream(a);
}

ptr_t audio_stream_impl(doremir_id_t interface)
{
    static doremir_string_show_t audio_stream_show_impl
        = { audio_stream_show };
    static doremir_destroy_t audio_stream_destroy_impl
        = { audio_stream_destroy };

    switch (interface) {
    case doremir_string_show_i:
        return &audio_stream_show_impl;

    case doremir_destroy_i:
        return &audio_stream_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

void doremir_audio_engine_log_error_from(doremir_string_t msg, doremir_string_t origin);

error_t audio_device_error(string_t msg)
{
    return doremir_error_create_simple(error,
                                       msg,
                                       string("Doremir.Device.Audio"));
}

error_t audio_device_error_with(string_t msg, int code)
{
    return doremir_error_create_simple(error,
                                       string_dappend(msg, format_integer(" (error code %d)", code)),
                                       string("Doremir.Device.Audio"));
}

void audio_device_fatal(string_t msg, int code)
{
    doremir_audio_engine_log_error_from(
        string_dappend(msg, format_integer(" (error code %d)", code)),
        string("Doremir.Device.Audio"));

    doremir_audio_engine_log_error(string("Terminating Audio Engine"));
    exit(error);
}

