
# Devices {#Devices}

@anchor Devices
@tableofcontents

Devices are the entities that allow the Audio Engine to communicate with the
outside world. Any client will need to connect at least two devices to each other
to form a audio stream. While signals and processors denote functions, devices
denote sources and sinks of audio data, such as files, memory buffers or audio
hardware. Typically, each physical audio or midi interface is represented by a
single device in the Audio Engine. The operating system may also provide abstract
devices, representing network connections, software mixers and the like.

Devices are grouped into *real-time devices*, *non-real-time devices*. Audio and
midi information are handled by different devices. The Audio Engine does not
provide non-real-time midi at the moment: if you need to parse and process a midi
file, use a different library.

# Real time devices {#rt}

## Sessions and streams {#sessionsandstreams}

The Audio Engine provides access to devices through the concepts of *sessions*, and
*streams*. While a *device* provides access to an external audio interface, a
*session* provides access to the entire audio system, and a *stream* to a specific
audio computation. These concepts are hierarchical, each stream is associated with
a device and each device with a session.

The Audio Engine places certain restrictions on the order or acquisition of
sessions, devices and streams. Any client that wants to obtain a device must first
initiate a session. The initialization of a session may fail, for example if the
underlying audio system is already being used by an exclusive process. If it
succeeds, a handle to the underlying session is provided. This handle allow the
user to inspect the set of devices available in the sesssion. The client may then
open a stream ... 

TODO

@image html  device_states.png "State transactions of the audio system (simplified)"
@image latex device_states.pdf "State transactions of the audio system (simplified)" width=0.8\textwidth

@note
    The semantics of *streams* have been changed from earlier versions of the Audio Engine, in which a *stream*
    could be repeatedly stopped and started.


## Session notifications {#notifications}

Sessions represent a snapshot of the setup at the time it was initiated; the set of
available devices in a specific session will never change. If a change in the
underlying audio system is detected while a session is still active, a new session
has to be started to observe the new setup.

TODO

## Audio streams {#audiostreams}

### Acquire-release style {#aqaudio}

To use a real-time device in imperative fashion, the typical paired method pattern
should be used. You call a creation method to get a session or stream, and a
destruction method to release it after use.

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/audio.h>

typedef doremir_device_audio_t         device_t;
typedef doremir_device_audio_session_t session_t;
typedef doremir_device_audio_stream_t  stream_t_;
typedef doremir_processor_t            processor_t;

int main (int argc, char const *argv[])
{     
  session_t session;
  session = doremir_device_audio_begin_session();

  if (doremir_check(session)) {
    doremir_error_log(session);
    return;
  }

  {
    device_t    input, output;
    processor_t proc;
    stream_t    stream
    
    proc    = doremir_processor_identity();
    input   = doremir_device_audio_default(session)->first;
    output  = doremir_device_audio_default(session)->second;    
    stream  = doremir_device_audio_start_stream(input, proc, output);

    if (doremir_check(stream)) {
      doremir_error_log(stream);
      exit(-1);
    }

    doremir_thread_sleep(doremir_seconds(10));
    
    doremir_device_audio_stop_stream(stream);
    doremir_destroy(proc);
  }

  doremir_device_audio_end_session(session);
}
~~~~


### Callback style {#cbaudio}

The callback style API use inversion of control to hide acquire-release pattern. You provide a
callback to be invoked when the session or stream is valid, and the destruction is handled automatically
after this method has returned. Errors are handled by a special callback, to which you can pass
[Doremir.Error.log](@ref doremir_error_log), or any user defined function.

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/audio.h>

typedef doremir_ptr_t                   ptr_t;
typedef doremir_device_audio_t          device_t;
typedef doremir_device_audio_session_t  session_t;
typedef doremir_device_audio_stream_t   stream_t_;
typedef doremir_processor_t             processor_t;

stream_t run_callback(stream_t stream)
{
  doremir_thread_sleep(doremir_seconds(10));
  return stream;
}

session_t session_callback(ptr_t data, session_t session)
{
  device_t  input, output;
  processor_t proc;

  proc    = doremir_processor_identity();
  input   = doremir_device_audio_default(session)->first;
  output  = doremir_device_audio_default(session)->second;    

  doremir_device_audio_with_stream(devices.first, processor, devices.second,
    run_callback, doremir_error_log, NULL);

  doremir_destroy(proc);
  return session;
}

int main (int argc, char const *argv[])
{
  doremir_device_audio_with_session(session_callback, NULL, doremir_error_log, NULL);
}
~~~~


# Non-realtime devices {#nrt}

## The run method {#run}


## File devices {#file}

### Acquire-release style {#aqfile}

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/file.h>

typedef doremir_device_file_t  device_t;
typedef doremir_processor_t    processor_t;

int main (int argc, char const *argv[])
{
  device_t    input, output;
  processor_t proc;
  future_t    result;

  proc    = doremir_processor_identity();
  input   = doremir_device_file_open(doremir_str("test/in.wav"));
  output  = doremir_device_file_open(doremir_str("test/out.wav"));

  result  = doremir_device_file_run(in, out);

  if (doremir_check(stream)) {
    doremir_error_log(stream);
    exit(-1);
  }

  doremir_device_file_wait(stream);
}
~~~~

### Callback style {#cbfile}

TODO


## Buffer devices {#buffer}

### Acquire-release style {#aqbuffer}

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/file.h>

typedef doremir_device_file_t  device_t;
typedef doremir_processor_t    processor_t;

int main (int argc, char const *argv[])
{
  device_t    input, output;
  processor_t proc;
  future_t    result;

  proc    = doremir_processor_identity();
  input   = doremir_device_buffer_open(doremir_buffer_create(1024));
  output  = doremir_device_buffer_open(doremir_buffer_create(1024));

  result = doremir_device_buffer_run(in, out);

  if (doremir_check(stream)) {
    doremir_error_log(stream);
    exit(-1);
  }

  doremir_device_buffer_wait(stream);
}
~~~~


### Callback style {#cbbuffer}

TODO





