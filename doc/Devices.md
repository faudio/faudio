
# Devices {#Devices}

@anchor Devices
@tableofcontents

@note
    This page is under construction.

Devices are the entities that allow the Audio Engine to communicate with the
outside world. Any client will need to connect at least two devices to each other
to form a audio stream. While signals and processors denote functions, devices
denote sources and sinks of audio data, such as files, memory buffers or audio
hardware.

Devices are grouped into *real-time devices*, *non-real-time devices*. Audio and
midi information are handled by different devices. Note that the Audio Engine does not
provide non-real-time midi at the moment: if you need to parse and process a midi
file you must use some other method.


# Real time devices {#RealTime}

## Sessions and streams {#SessionsAndStreams}

The Audio Engine provides access to real-time devices through of *sessions*, and
*streams*. While a *device* provides access to an external audio interface, a
*session* provides access to the entire audio system, and a *stream* to a specific
audio computation. These concepts are hierarchical, each stream is associated with
a device and each device with a session.

Typically, each physical audio or midi interface is represented by a
single device in the Audio Engine. The operating system may also provide abstract
devices, representing network connections, software mixers and the like.

The Audio Engine places certain restrictions on the order or acquisition of
sessions, devices and streams. Any client that wants to obtain a device must first
initiate a session. The initialization of a session may fail, for example if the
underlying audio system is already being used by an exclusive process. If it
succeeds, a handle to the underlying session is provided. This handle allow the
user to inspect the set of devices available in the sesssion. The client may then
open a stream.

TODO

@image html  device_states.png "State transactions of the audio system (simplified)"
@image latex device_states.pdf "State transactions of the audio system (simplified)" width=0.8\textwidth

@note
    The semantics of *streams* have been changed from earlier versions of the Audio Engine, in which a *stream*
    could be repeatedly stopped and started.


## Session notifications {#Notifications}

Sessions represent a snapshot of the setup at the time it was initiated; the set of
available devices in a specific session will never change. If a change in the
underlying audio system is detected while a session is still active, a new session
has to be started to observe the new setup.

You can register a callback to be invoked when the a possible change in hardware
setup is detected, see @ref fae_device_audio_set_status_callback. Note that
this callback may be invoked in an interrupt handler thread and that the task of
ending a session should generally be handled in the same thread that created the
session. Use an atomic reference or a condition variable to communicate the
notification to the appropriate thread.


## Audio streams {#AudioStreams}

### Acquire-release style {#AudioAR}

The acquire-release style use a paired method pattern. You call a creation method
to get a value, and a destruction method to release. Note that devices, sessions and
streams have single-ownership semantics.

~~~~
#include <fae/device/audio.h>
#include <fae/thread.h>
#include <fae/util.h>

int main (int argc, char const *argv[])
{
    session_t       session;
    device_t        input, output;
    processor_t     proc;
    stream_t        stream
    
    // Processor to use
    proc = fae_processor_identity(type_pair(type(f32), type(f32)));
    
    // Begin session
    session = fae_device_audio_begin_session();

    // Handle possible error
    if (fae_check(stream)) {
        fae_error_log(stream);
        goto cleanup;
    }

    // Session obtained, we can now access devices
    input  = fae_device_audio_default_input(session);
    input  = fae_device_audio_default_output(session);
    
    // Start stream
    stream = fae_device_audio_open_stream(input, proc, output);

    // Handle possible error
    if (fae_check(stream)) {
        fae_error_log(stream);
        goto cleanup;
    }

    // Stream active, let it run for 5 seconds
    fae_thread_sleep(5000);

    // Cleanup
cleanup:
    fae_device_audio_close_stream(stream);
    fae_device_audio_end_session(session);
    fae_destroy(proc);
}
~~~~


### Callback style {#AudioCB}

The callback style require that you provide a callback to be invoked when the
session or stream is valid. Destruction is handled automatically after this method
has returned. Errors are handled by a special callback, to which you can pass
[fae_error_log](@ref fae_error_log), or a user defined function.

~~~~
#include <fae/time.h>
#include <fae/thread.h>
#include <fae/device/audio.h>

stream_t run_callback(stream_t stream)
{
    // Stream active, let it run for 5 seconds
    fae_thread_sleep(fae_seconds(10));
    return stream;
}

session_t session_callback(void* data, session_t session)
{
    device_t    input, output;
    processor_t proc;

    // Session obtained, we can now access devices
    input  = fae_device_audio_default_input(session);
    output = fae_device_audio_default_input(session);
    proc    = (processor_t*) data;

    // Start stream
    fae_device_audio_with_stream(
        input, proc, output,
        run_callback, fae_error_log, NULL
    );

    fae_destroy(proc);
    return session;
}

int main (int argc, char const *argv[])
{                  
    // Processor to use
    processor_t proc = fae_processor_identity(type_pair(type(f32), type(f32)));
    
    // Begin session
    fae_device_audio_with_session(
        session_callback, proc,
        fae_error_log, NULL
    );
}
~~~~


# Non-realtime devices {#id8127832}

## The run method {#id98281723}

TODO

## File devices {#id9192746}

~~~~
#include <fae/time.h>
#include <fae/thread.h>
#include <fae/device/file.h>

typedef fae_device_file_t   device_t;
typedef fae_processor_t     processor_t;

int main (int argc, char const *argv[])
{
    device_t    input, output;
    processor_t proc;
    result_t    result;

    // Processor to use
    proc    = fae_processor_identity(type_pair(type(f32), type(f32)));

    // Open streams
    input   = fae_device_file_open(string("test/in.wav"));
    output  = fae_device_file_open(string("test/out.wav"));

    // Handle possible errors
    if (fae_check(input)) {
        fae_error_log(result);
    }                                    
    
    if (fae_check(output)) {
        fae_error_log(result);
    }                                    

    result  = fae_device_file_run(in, proc, out);

    // Handle possible error
    if (fae_check(result)) {
        fae_error_log(result);
    }                                    
    
    fae_device_buffer_destroy(input);
    fae_device_buffer_destroy(output);;
}
~~~~


## Buffer devices {#id11127283}

~~~~
#include <fae/time.h>
#include <fae/thread.h>
#include <fae/device/file.h>

typedef fae_device_file_t   device_t;
typedef fae_processor_t     processor_t;

int main (int argc, char const *argv[])
{
    device_t    input, output;
    processor_t proc;
    result_t    result;

    // Processor to use
    proc    = fae_processor_identity(type_pair(type(f32), type(f32)));

    // Open streams
    input   = fae_device_buffer_open(fae_buffer_create(1024));
    output  = fae_device_buffer_open(fae_buffer_create(1024));

    // Handle possible errors
    if (fae_check(input)) {
        fae_error_log(result);
    }                                    
    
    if (fae_check(output)) {
        fae_error_log(result);
    }                                    

    result  = fae_device_buffer_run(in, proc, out);

    // Handle possible error
    if (fae_check(result)) {
        fae_error_log(result);
    }                                    
    
    fae_device_buffer_close(input);
    fae_device_buffer_close(output);
}
~~~~





