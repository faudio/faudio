
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/audio.h>

#include <fa/atomic.h> // TODO improving
#include <fa/thread.h>
#include <fa/util.h>
#include <fa/time.h>

#include <portaudio.h>

/*
    Notes:
        * Name mixup. What to call incoming/outcoming to make it unambigous.
            * to_audio, from_audio or something?
        * FIXME endSession must check for running streams, or we get consistency errors
            * Either abort streams or refuse to close session
 */


typedef fa_audio_device_t           device_t;
typedef fa_audio_stream_t           stream_t;
typedef fa_audio_session_t          session_t;
typedef fa_audio_stream_callback_t  stream_callback_t;
typedef fa_audio_session_callback_t session_callback_t;
typedef fa_audio_status_callback_t  status_callback_t;

typedef PaDeviceIndex native_index_t;
typedef PaStream     *native_stream_t;

struct _fa_audio_session_t {

    impl_t              impl;               // Dispatcher
    system_time_t       acquired;           // Time of acquisition (not used at the moment)

    list_t              devices;            // Cached device list

    device_t            def_input;          // Default devices, both possibly null
    device_t            def_output;         // If present, these are also in the above list
};

struct _fa_audio_device_t {

    impl_t              impl;               // Dispatcher
    native_index_t      index;              // Native device index

    string_t            name;               // Cached names
    string_t            host_name;

    // bool                muted;              // Not used at the moment
    // double              volume;
};

struct _fa_audio_stream_t {

    impl_t              impl;               // Dispatcher
    native_stream_t     native;             // Native stream

    device_t            input, output;

    unsigned            input_channels, output_channels;
    double              sample_rate;
    long                max_buffer_size;
    int32_t             sample_count;       // Monotonically increasing sample count
};

static mutex_t pa_mutex;
static bool    pa_status;

error_t audio_device_error(string_t msg);
error_t audio_device_error_with(string_t msg, int error);
ptr_t audio_session_impl(fa_id_t interface);
ptr_t audio_device_impl(fa_id_t interface);
ptr_t audio_stream_impl(fa_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(native_index_t index);
inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t input, device_t output, ptr_t proc, double sample_rate, long max_buffer_size);
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
    session_t session = fa_new(audio_session);
    session->impl = &audio_session_impl;
    return session;
}

inline static void session_init_devices(session_t session)
{
    native_index_t count;
    list_t         devices;

    count   = Pa_GetDeviceCount();
    devices = fa_list_empty();

    for (size_t i = 0; i < count; ++i) {
        device_t device = new_device(i);

        if (device) {
            devices = fa_list_dcons(device, devices);
        }
    }

    session->devices      = fa_list_dreverse(devices);
    session->def_input    = new_device(Pa_GetDefaultInputDevice());
    session->def_output   = new_device(Pa_GetDefaultOutputDevice());
}

inline static void delete_session(session_t session)
{
    // TODO free device list
    fa_delete(session);
}

inline static device_t new_device(native_index_t index)
{
    if (index == paNoDevice) {
        return NULL;
    }

    device_t device = fa_new(audio_device);
    device->impl    = &audio_device_impl;

    const PaDeviceInfo  *info      = Pa_GetDeviceInfo(index);
    const PaHostApiInfo *host_info = Pa_GetHostApiInfo(info->hostApi);

    device->index       = index;
    device->name        = string((char *) info->name);      // const cast
    device->host_name   = string((char *) host_info->name);
    // device->muted       = false;
    // device->volume      = 1.0;

    return device;
}

inline static void delete_device(device_t device)
{
    fa_destroy(device->name);
    fa_destroy(device->host_name);
    fa_delete(device);
}

inline static stream_t new_stream(device_t input, device_t output, ptr_t proc, double sample_rate, long max_buffer_size)
{
    stream_t stream         = fa_new(audio_stream);

    stream->impl            = &audio_stream_impl;

    stream->input           = input;
    stream->output          = output;
    stream->input_channels  = num_input_channels(input);
    stream->output_channels = num_output_channels(output);

    stream->sample_rate     = sample_rate;
    stream->max_buffer_size = max_buffer_size;

    // stream->proc            = proc;
    // stream->proc_impl       = fa_interface(fa_processor_interface_i, stream->proc);
    // assert(stream->proc_impl && "Must implement Processor");

    stream->sample_count    = 0;

    // stream->incoming        = (sender_t)   lockfree_dispatcher();
    // stream->outgoing        = (receiver_t) lockfree_dispatcher();

    return stream;
}

inline static void delete_stream(stream_t stream)
{
    // fa_destroy(stream->incoming);
    // fa_destroy(stream->outgoing);
    fa_delete(stream);
}


// --------------------------------------------------------------------------------

void fa_audio_initialize()
{
    pa_mutex  = fa_thread_create_mutex();
    pa_status = false;
}

void fa_audio_terminate()
{
    fa_thread_destroy_mutex(pa_mutex);
}

// --------------------------------------------------------------------------------

session_t fa_audio_begin_session()
{
    if (!pa_mutex) {
        assert(false && "Module not initalized");
    }

    inform(string("Initializing real-time audio session"));

    fa_thread_lock(pa_mutex);
    {
        if (pa_status) {
            fa_thread_unlock(pa_mutex);
            return (session_t) audio_device_error(string("Overlapping real-time audio sessions"));
        } else {
            Pa_Initialize();
            pa_status = true;
            fa_thread_unlock(pa_mutex);

            session_t session = new_session();
            session_init_devices(session);
            // session->acquired = time(NULL);      // TODO
            return session;
        }
    }
}

void fa_audio_end_session(session_t session)
{
    if (!pa_mutex) {
        assert(false && "Module not initalized");
    }

    inform(string("Terminating real-time audio session"));

    fa_thread_lock(pa_mutex);
    {
        if (pa_status) {
            Pa_Terminate();
            pa_status = false;
        }
    }
    fa_thread_unlock(pa_mutex);
    delete_session(session);
}

void fa_audio_with_session(
    session_callback_t session_callback,
    ptr_t                           session_data,
    error_callback_t                error_callback,
    ptr_t                           error_data
)
{
    session_t session = fa_audio_begin_session();

    if (fa_check(session)) {
        error_callback(error_data, (error_t) session);
    } else {
        session_callback(session_data, session);
    }

    fa_audio_end_session(session);
}

fa_list_t fa_audio_all(session_t session)
{
    return fa_copy(session->devices);
}

fa_pair_t fa_audio_default(session_t session)
{
    return pair(session->def_input, session->def_output);
}

device_t fa_audio_default_input(session_t session)
{
    return session->def_input;
}

device_t fa_audio_default_output(session_t session)
{
    return session->def_output;
}

fa_string_t fa_audio_name(device_t device)
{
    return fa_copy(device->name);
}

fa_string_t fa_audio_host_name(device_t device)
{
    return fa_copy(device->host_name);
}

type_t fa_audio_input_type(device_t device)
{
    const PaDeviceInfo *info = Pa_GetDeviceInfo(device->index);
    return fa_type_repeat(info->maxInputChannels, type_frame(type(f32)));
}

type_t fa_audio_output_type(device_t device)
{
    const PaDeviceInfo  *info = Pa_GetDeviceInfo(device->index);
    return fa_type_repeat(info->maxOutputChannels, type_frame(type(f32)));
}

bool fa_audio_has_input(device_t device)
{
    return fa_not_equal(fa_audio_input_type(device), type(unit));
}
bool fa_audio_has_output(device_t device)
{
    return fa_not_equal(fa_audio_output_type(device), type(unit));
}


void add_audio_status_listener(status_callback_t function, ptr_t data);

void fa_audio_set_status_callback(
    status_callback_t function,
    ptr_t             data,
    session_t         session)
{
    assert(session && "Not a real session");

    // See device_osx.c and device_win.c
    add_audio_status_listener(function, data);
}

// --------------------------------------------------------------------------------

static inline int num_input_channels(device_t device)
{
    return device ? fa_type_channels(fa_audio_input_type(device)) : 0;
}
static inline int num_output_channels(device_t device)
{
    return device ? fa_type_channels(fa_audio_output_type(device)) : 0;
}

// static inline size_t get_max_buffer_size(device_t device)
// {
//     const PaDeviceInfo  *info      = Pa_GetDeviceInfo(device->index);
//     const PaHostApiInfo *host_info = Pa_GetHostApiInfo(info->hostApi);
//     long min, max, prefered, granularity;
//     PaError err = paNoError;
//
//     switch(host_info->type)
//     {
//     case paCoreAudio: {
//         // error
//         err = PaMacCore_GetBufferSizeRange(device->index, min, max);
//         assert(err == paNoError);
//         return max;
//     }
//     case paASIO: {
//         err = PaAsio_GetAvailableBufferSizes(device->index, min, max, prefered, granularity);
//         assert(err == paNoError);
//         return max;
//     }
//     default:
//         return 0;
//     }
// }

void audio_inform_opening(device_t input, ptr_t proc, device_t output)
{
    inform(string("Opening real-time audio stream"));
    inform(string_dappend(string("    Input:  "), input ? fa_string_show(input) : string("-")));
    inform(string_dappend(string("    Output: "), output ? fa_string_show(output) : string("-")));
    inform(string_dappend(string("    Processor: "), proc ? fa_string_show(proc) : string("-")));
}

// TODO change sample rate
// TODO use unspec vector size if we can determine max
stream_t fa_audio_open_stream(device_t input, ptr_t proc, device_t output)
{
    PaError         status;
    unsigned long   buffer_size = 256;
    double          sample_rate = 44100;
    stream_t        stream      = new_stream(input, output, proc, sample_rate, buffer_size);

    audio_inform_opening(input, proc, output);
    {
        // TODO
        // if (fa_not_equal(
        //             fa_processor_input_type(proc),
        //             fa_audio_input_type(input))) {
        //     char msg[100];
        //     snprintf(msg, 100, "Could not connect device %s to processor %s",
        //              unstring(fa_string_show(fa_audio_input_type(input))),
        //              unstring(fa_string_show(proc)));
        //     error_t err = fa_error_create_simple(error,
        //                                               string(msg),
        //                                               string("Doremir.Device.Audio"));
        //     return (stream_t) err;
        // }
    }
    {
        // if (fa_not_equal(
        //             fa_processor_output_type(proc),
        //             fa_audio_output_type(output))) {
        //     char msg[100];
        //     snprintf(msg, 100, "Could not connect processor %s to device %s",
        //              unstring(fa_string_show(proc)),
        //              unstring(fa_string_show(fa_audio_output_type(output))));
        //     error_t err = fa_error_create_simple(error,
        //                                               string(msg),
        //                                               string("Doremir.Device.Audio"));
        //     return (stream_t) err;
        // }
    }
    {
        PaStreamParameters inp = {
            .suggestedLatency           = 0,
            .hostApiSpecificStreamInfo  = NULL,
            .device                     = (input ? input->index : 0),
            .sampleFormat               = (paFloat32 | paNonInterleaved),
            .channelCount               = stream->input_channels
        };

        PaStreamParameters outp = {
            .suggestedLatency           = 0,
            .hostApiSpecificStreamInfo  = NULL,
            .channelCount               = stream->output_channels,
            .sampleFormat               = (paFloat32 | paNonInterleaved),
            .device                     = (output ? output->index : 0)
        };

        const PaStreamParameters       *in       = input ? &inp : NULL;
        const PaStreamParameters       *out      = output ? &outp : NULL;
        PaStreamFlags                   flags    = paNoFlag;
        PaStreamCallback               *callback = native_audio_callback;
        ptr_t                           data     = stream;

        status = Pa_OpenStream(&stream->native, in, out, sample_rate, buffer_size, flags, callback, data);

        if (status != paNoError) {
            return (stream_t) audio_device_error_with(string("Could not start stream"), status);
        }

        status = Pa_SetStreamFinishedCallback(stream->native, native_finished_callback);

        if (status != paNoError) {
            return (stream_t) audio_device_error_with(string("Could not start stream"), status);
        }
    }
    {
        before_processing(stream);

        status = Pa_StartStream(stream->native);

        if (status != paNoError) {
            return (stream_t) audio_device_error_with(string("Could not start stream"), status);
        }
    }

    return stream;
}

void fa_audio_close_stream(stream_t stream)
{
    inform(string("Closing real-time audio stream"));

    // Note that after_processing will be called from native_finished_callback
    Pa_CloseStream(stream->native);
    delete_stream(stream);
}

void fa_audio_with_stream(device_t            input,
                          ptr_t         processor,
                          device_t            output,
                          stream_callback_t   stream_callback,
                          ptr_t               stream_data,
                          error_callback_t    error_callback,
                          ptr_t               error_data)
{
    stream_t stream = fa_audio_open_stream(input, processor, output);

    if (fa_check(stream)) {
        error_callback(error_data, (error_t) stream);
    } else {
        stream_callback(stream_data, stream);
    }

    fa_audio_close_stream(stream);
}


// --------------------------------------------------------------------------------


void before_processing(stream_t stream)
{
    // fa_processor_info_t info = {
    //     .sample_rate = stream->sample_rate,
    //     .frame_size  = stream->max_buffer_size,
    //     .sample_time = stream->sample_count,
    //     .total_time  = NULL, // TODO
    //     .dispatcher  = (dispatcher_t) stream->incoming
    // };


    size_t sz = 1204 * 1204 * sizeof(double);
    void *b = malloc(sz);
    memset(b, 0, sz);

    // allocate buffers
    // stream->proc_impl->before(stream->proc, &info);
}

void after_processing(stream_t stream)
{
    // fa_processor_info_t info = {
    //     .sample_rate = stream->sample_rate,
    //     .frame_size  = stream->max_buffer_size,
    //     .sample_time = stream->sample_count,
    //     .total_time  = NULL, // TODO
    //     .dispatcher  = (dispatcher_t) stream->incoming
    // };

    // stream->proc_impl->after(stream->proc, &info);
    // free buffers
}

int during_processing(stream_t stream, unsigned count, float **input, float **output)
{


    stream->sample_count += count; // TODO atomic incr
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
    // if (flags)
    // {
    //     if (flags & paInputUnderflow)  warn(string("Input underflow"));
    //     if (flags & paInputOverflow)   warn(string("Input overflow"));
    //     if (flags & paOutputUnderflow) warn(string("Output underflow"));
    //     if (flags & paOutputOverflow)  warn(string("Output overflow"));
    //     if (flags & paPrimingOutput)   warn(string("Priming output"));
    // }
    return during_processing(data, count, (float **) input, (float **) output);
}

void native_finished_callback(void *data)
{
    after_processing(data);
}


// --------------------------------------------------------------------------------

bool audio_session_equal(ptr_t a, ptr_t b)
{
    return a == b;
}

fa_string_t audio_session_show(ptr_t a)
{
    string_t str = string("<AudioSession ");
    str = string_dappend(str, fa_string_format_integral("%p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void audio_session_destroy(ptr_t a)
{
    fa_audio_end_session(a);
}

ptr_t audio_session_impl(fa_id_t interface)
{
    static fa_equal_t audio_session_equal_impl
        = { audio_session_equal };
    static fa_string_show_t audio_session_show_impl
        = { audio_session_show };
    static fa_destroy_t audio_session_destroy_impl
        = { audio_session_destroy };

    switch (interface) {
    case fa_equal_i:
        return &audio_session_equal_impl;

    case fa_string_show_i:
        return &audio_session_show_impl;

    case fa_destroy_i:
        return &audio_session_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool audio_device_equal(ptr_t a, ptr_t b)
{
    device_t device1 = (device_t) a;
    device_t device2 = (device_t) b;
    // TODO check that session is valid
    return device1->index == device2->index;
}

fa_string_t audio_device_show(ptr_t a)
{
    device_t device = (device_t) a;

    string_t str = string("<AudioDevice ");
    str = string_dappend(str, fa_audio_host_name(device));
    str = string_dappend(str, string(" "));
    str = string_dappend(str, fa_audio_name(device));
    str = string_dappend(str, string(">"));
    return str;
}

ptr_t audio_device_impl(fa_id_t interface)
{
    static fa_equal_t audio_device_equal_impl
        = { audio_device_equal };
    static fa_string_show_t audio_device_show_impl
        = { audio_device_show };

    switch (interface) {
    case fa_equal_i:
        return &audio_device_equal_impl;

    case fa_string_show_i:
        return &audio_device_show_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

fa_string_t audio_stream_show(ptr_t a)
{
    string_t str = string("<AudioStream ");
    str = string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void audio_stream_destroy(ptr_t a)
{
    fa_audio_close_stream(a);
}


ptr_t audio_stream_impl(fa_id_t interface)
{
    static fa_string_show_t audio_stream_show_impl
        = { audio_stream_show };
    static fa_destroy_t audio_stream_destroy_impl
        = { audio_stream_destroy };

    switch (interface) {


    case fa_string_show_i:
        return &audio_stream_show_impl;

    case fa_destroy_i:
        return &audio_stream_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

void fa_fa_log_error_from(fa_string_t msg, fa_string_t origin);

error_t audio_device_error(string_t msg)
{
    return fa_error_create_simple(error,
                                  msg,
                                  string("Doremir.Device.Audio"));
}

error_t audio_device_error_with(string_t msg, int code)
{
    return fa_error_create_simple(error,
                                  string_dappend(msg, format_integral(" (error code %d)", code)),
                                  string("Doremir.Device.Audio"));
}

void audio_device_fatal(string_t msg, int code)
{
    fa_fa_log_error_from(
        string_dappend(msg, format_integral(" (error code %d)", code)),
        string("Doremir.Device.Audio"));

    fa_fa_log_error(string("Terminating Fa"));
    exit(error);
}

