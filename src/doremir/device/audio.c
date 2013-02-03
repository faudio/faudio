
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

typedef PaStream     *native_stream_t;
typedef PaDeviceIndex native_device_t;

struct _doremir_device_audio_session_t {
    impl_t          impl;               // Dispatcher
    long            acquired;
};

struct _doremir_device_audio_t {
    impl_t          impl;               // Dispatcher
    bool            muted;
    double          volume;
    native_device_t native;
};

struct _doremir_device_audio_stream_t {
    impl_t          impl;               // Dispatcher

    device_t        input, output;
    processor_t     proc;
    atomic_t        time;
    dispatcher_t    in_disp;
    dispatcher_t    out_disp;

    native_stream_t native;
};

static mutex_t pa_mutex;
static bool    pa_status;

error_t audio_device_error(string_t msg);

// --------------------------------------------------------------------------------

inline static session_t new_session()
{    
    session_t session = doremir_new(device_audio_session);
    // session->impl = &session_impl;
    // TODO
    return session;
}
inline static void delete_session(session_t session)
{
    doremir_delete(session);    
}

inline static device_t new_device()
{    
    device_t device = doremir_new(device_audio);
    // device->impl = &device_impl;
    // TODO
    return device;
}
inline static void delete_device(device_t device)
{    
    doremir_delete(device);    
}

inline static stream_t new_stream()
{    
    stream_t stream = doremir_new(device_audio_stream);
    // stream->impl = &stream_impl;
    // TODO
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

// FIXME return Optional?
session_t doremir_device_audio_begin_session()
{
    if (!pa_mutex) {
        assert(false && "Not initalized");
    }

    inform(string("Beginning audio session"));

    doremir_thread_lock(pa_mutex);
    {
        if (pa_status) {
            audio_device_error(string("Concurrent audio sessions"));
        } else {
            Pa_Initialize();
            pa_status = true;
            // TODO create and return session object
        }
    }
    doremir_thread_unlock(pa_mutex);
    // TODO return
    return NULL;
}

void doremir_device_audio_end_session(session_t session)
{
    if (!pa_mutex) {
        assert(false && "Not initalized");
    }

    doremir_thread_lock(pa_mutex);
    {
        if (pa_status) {
            Pa_Terminate();
            pa_status = false;
        }
    }
    doremir_thread_unlock(pa_mutex);
}

void doremir_device_audio_with_session(
    doremir_device_audio_session_callback_t session_callback,
    doremir_ptr_t                           session_data,
    doremir_error_callback_t          error_callback,
    doremir_ptr_t                           error_data
)
{
    // session_t session = doremir_device_audio_begin_session();
    // callback(session);
    // doremir_device_audio_end_session(session);
}

doremir_list_t doremir_device_audio_all(session_t session)
{
    assert(false && "Not implemented");
}

// FIXME return Optional?
doremir_pair_t doremir_device_audio_default(session_t session)
{
    assert(false && "Not implemented");
}

doremir_string_t doremir_device_audio_name(device_t device)
{
    assert(false && "Not implemented");
}

doremir_string_t doremir_device_audio_host_name(device_t device)
{
    assert(false && "Not implemented");
}

bool doremir_device_audio_has_input(device_t device)
{
    assert(false && "Not implemented");
}

bool doremir_device_audio_has_output(device_t device)
{
    assert(false && "Not implemented");
}

doremir_pair_t doremir_device_audio_channels(device_t device)
{
    assert(false && "Not implemented");
}



// --------------------------------------------------------------------------------

// FIXME return Optional?
stream_t doremir_device_audio_open_stream(device_t    input,
        processor_t processor,
        device_t    output)
{
    assert(false && "Not implemented");
    // call Pa_OpenStream
    // call before_processing
    // call Pa_StartStream
}

stream_t doremir_device_audio_restart_stream(stream_t stream)
{
    assert(false && "Not implemented");
}

void doremir_device_audio_close_stream(stream_t stream)
{
    assert(false && "Not implemented");
    // call Pa_StopStream
    // (after_processing is called from the finished callback)
    // call Pa_CloseStream
}

void doremir_device_audio_with_stream(
    doremir_device_audio_t                 input,
    doremir_processor_t                    processor,
    doremir_device_audio_t                 output,
    doremir_device_audio_stream_callback_t stream_callback,
    doremir_ptr_t                          stream_data,
    doremir_error_callback_t         error_callback,
    doremir_ptr_t                          error_data
)
{
    stream_t stream = doremir_device_audio_open_stream(input, processor, output);
    // doremir_check(stream) ? callback(stream) : error_callback(stream);
    doremir_device_audio_close_stream(stream);
}


// --------------------------------------------------------------------------------

void before_processing(stream_t stream)
{
    // extract top-level audio type
    // allocate buffers
    // create message allocator
    // register all processors as receivers on the incoming message dispatcher
    // call setup() on top processor (passing outgoing message receiver)
}

void after_processing(stream_t stream)
{
    // call cleanup() on top processor
    // unregister processors from incoming message dispatcher
    // destroy message allocator
    // free buffers
}

int during_processing(stream_t stream)
{
    // update stream time
    // call dispatch() on the innner dispatcher

    // deliver inputs
    // call process() on top processor
    // deliver outputs

    // decide whether to continue
    return 0; // TODO
}


/* The callbacks */

int pa_main_callback(const void                       *input_ptr,
                     void                             *output_ptr,
                     unsigned long                     frame_count,
                     const PaStreamCallbackTimeInfo   *time_info,
                     PaStreamCallbackFlags             flags,
                     void                             *data)
{
    // call during_processing
    return 0;
}

void pa_finished_callback(void *userData)
{
    // call after_processing
}


// --------------------------------------------------------------------------------

void doremir_audio_engine_log_error_from(doremir_string_t msg, doremir_string_t origin);

error_t audio_device_error(string_t msg)
{
    return doremir_error_create_simple(error, msg, string("Doremir.Device.Audio"));
}

void audio_device_fatal(char *msg, int error)
{
    doremir_audio_engine_log_error_from(
        string_dappend(string(msg), format_integer(" (error code %d)", error)), 
        string("Doremir.Device.Audio"));
    doremir_audio_engine_log_error(string("Terminating Audio Engine"));
    exit(error);
}

