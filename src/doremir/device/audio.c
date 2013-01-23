
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

static void fatal(char *msg, int error);

session_t doremir_device_audio_begin_session()
{
  if (!pa_mutex) {
    assert(false && "Not initalized");
  }

  doremir_thread_lock(pa_mutex);
  {
    if (pa_status) {
      // TODO concurrent session error
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

session_t doremir_device_audio_restart_session(session_t session)
{
  assert(false && "Not implemented");
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

void doremir_device_audio_with_session(session_callback_t callback)
{
  session_t session = doremir_device_audio_begin_session();
  callback(session);
  doremir_device_audio_end_session(session);
}

doremir_list_t doremir_device_audio_all(session_t session)
{
  assert(false && "Not implemented");
}

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

stream_t doremir_device_audio_start_stream(device_t    input,
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

void doremir_device_audio_stop_stream(stream_t stream)
{
  assert(false && "Not implemented");
  // call Pa_StopStream
  // (after_processing is called from the finished callback)
  // call Pa_CloseStream
}

void doremir_device_audio_with_stream(device_t          input,
                                      processor_t       processor,
                                      device_t          output,
                                      stream_callback_t callback)
{
  stream_t stream = doremir_device_audio_start_stream(input, processor, output);
  // doremir_check(stream) ? callback(stream) : error_callback(stream);
  doremir_device_audio_stop_stream(stream);
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

void fatal(char *msg, int error)
{
  // TODO log
  printf("Fatal error: Doremir: Device: Audio: %s: %d\n", msg, error);
  exit(error);
}
