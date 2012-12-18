
#include <doremir/device/audio.h>
#include <doremir/thread.h>
#include <portaudio.h>

struct _doremir_device_audio_session_t
{
    // nothing yet
};

struct _doremir_device_audio_t
{
    
};

struct _doremir_device_audio_stream_t
{
    
};

static void fatal(char* msg, int error);

static doremir_thread_mutex_t   pa_mutex    = NULL;
static bool                     pa_status   = false;

void doremir_device_audio_initialize()
{
    pa_mutex = doremir_thread_create_mutex();
}
void doremir_device_audio_terminate()
{
    doremir_thread_destroy_mutex(pa_mutex);
    pa_status = false;
}

// --------------------------------------------------------------------------------

/** Begin a new session.
 */
doremir_device_audio_session_t 
doremir_device_audio_begin_session()
{
    if (!pa_mutex) assert(false && "Not initalized");

    doremir_thread_lock(pa_mutex);
    {
        if (pa_status)
        {
            // TODO concurrent session error
        }
        else
        {
            Pa_Initialize();
            pa_status = true;
            // TODO create and return session object
        }
    }
    doremir_thread_unlock(pa_mutex);
}

/** Terminate the given session.
 */
void doremir_device_audio_end_session(doremir_device_audio_session_t session)
{
    if (!pa_mutex) assert(false && "Not initalized");

    doremir_thread_lock(pa_mutex);
    {
        if (pa_status)
        {
            Pa_Terminate();
            pa_status = false;
        }
    }
    doremir_thread_unlock(pa_mutex);
}

/** Get all active audio devices of the given session.
    @return
        A list of \ref doremir_device_audio_t.
 */
doremir_list_t doremir_device_audio_devices(doremir_device_audio_session_t session)
{
    assert(false && "Not implemented");
}

/** Get the standard devices of the given session.
    @return
        A pair of \ref doremir_device_audio_t.
 */
doremir_pair_t doremir_device_audio_standard(doremir_device_audio_session_t session)
{
    assert(false && "Not implemented");
}

/** Get the standard input device of the given session.
 */
doremir_device_audio_t doremir_device_audio_standard_input(doremir_device_audio_session_t session)
{             
    // doremir_pair_t std = doremir_device_audio_standard(session);
    // return (doremir_device_audio_t) std.fst;
    assert(false && "Not implemented");
}

/** Get the standard output device of the given session.
 */
doremir_device_audio_t doremir_device_audio_standard_output(doremir_device_audio_session_t session)
{
    // doremir_pair_t std = doremir_device_audio_standard(session);
    // return (doremir_device_audio_t) std.snd;
    assert(false && "Not implemented");
}

// --------------------------------------------------------------------------------

/** Open a stream on the given devices.
 */
doremir_device_audio_stream_t 
doremir_device_audio_open_stream(doremir_device_audio_t input,
                                 doremir_processor_t processor,
                                 doremir_device_audio_t output)
{
    assert(false && "Not implemented");
}

/** Close the given stream.
 */
void doremir_device_audio_close_stream(doremir_device_audio_stream_t stream)
{
    assert(false && "Not implemented");
}




// --------------------------------------------------------------------------------

void fatal(char* msg, int error)
{
    // TODO log
    printf("Fatal error: Doremir: Device: Audio: %s: %d\n", msg, error);
    exit(error);
}
