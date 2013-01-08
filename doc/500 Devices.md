
# Devices {#Devices}

[TOC]

# Real time {#RealTime}

The Audio Engine supports real-time audio and midi computations. The interface for these are similar but differs
in that audio streams use both [processors](@ref Processors) and [dispatchers](@ref Dispatchers), while midi 
streams use only dispatchers. 

See the \ref DoremirDeviceAudio and \ref DoremirDeviceMidi modules for details.

## Sessions and streams {#SessionDeviceStream}

The Audio Engine provides access to devices through the concepts of *sessions*, and *streams*. While a *device*
provides access to an external audio interface, a *session* provides access to the entire audio system, and a *stream*
to a specific audio computation. These concepts are hierarchical, each stream is associated with a device and each
device with a session.

@image html  device_states.png "Cap"
@image latex device_states.pdf "Cap" width=0.8\textwidth

@note
    The semantics of *streams* have been changed from earlier versions of the audio engine, in which a *stream* was
    a could be stopped and restarted. In the new implementation, streams are one-time entities like sessions.

## Acquire-release vs. callback {#DevicesStyles}

## Audio streams {#AudioStreams}

### Acquire-release style {#RealTimeAcquire-release}

To use a real-time device in imperative fashion, the typical paired method pattern should be used. You call 
a creation method to get a session or stream, and a destruction method to release it after use. 

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/audio.h>

typedef doremir_device_audio_t         device_t;
typedef doremir_device_audio_session_t session_t;
typedef doremir_device_audio_stream_t  stream_t_;
typedef doremir_processor_any_t        processor_t;

void test()
{       
    session_t session;
    session = doremir_device_audio_begin_session();

    if (doremir_check(session))
        doremir_print_error(session);

    {
        device_t    input, output;
        processor_t proc;
        stream_t    stream
        
        proc    = doremir_processor_identity();
        input   = doremir_device_audio_standard(session)->first;
        output  = doremir_device_audio_standard(session)->second;        
        stream  = doremir_device_audio_start_stream(input, proc, output);

        if (doremir_check(stream))
            doremir_print_error(stream);

        doremir_thread_sleep(doremir_seconds(10));
        doremir_device_audio_stop_stream(stream);
    }

    doremir_device_audio_end_session(session);
}
~~~~


### Callback style {#RealTimeCallback}

The callback style API use inversion of control to hide create–use-destroy pattern. You provide a
callback to be invoked when the session or stream is valid, and the destruction is handled automatically
after this method has returned. Errors are handled by a special callback, to which you can pass
doremir_print_error, or any user defined function.

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/audio.h>

typedef doremir_device_audio_t         device_t;
typedef doremir_device_audio_session_t session_t;
typedef doremir_device_audio_stream_t  stream_t_;
typedef doremir_processor_any_t        processor_t;

void run_callback(stream_t data)
{
    doremir_thread_sleep(doremir_seconds(10));
}

void session_callback(session_t session, doremir_ptr_t data)
{
    device_t    input, output;
    processor_t proc;

    proc    = doremir_processor_identity();
    input   = doremir_device_audio_standard(session)->first;
    output  = doremir_device_audio_standard(session)->second;        

    doremir_device_audio_with_stream(devices.first, processor, devices.second,
        run_callback, doremir_print_error, NULL);
}

void test()
{
    doremir_device_audio_with_session(session_callback, doremir_print_error, NULL);
}
~~~~


# Non-realtime {#NonRealTime}

## File streams {#FileStream}

### Acquire-release style {#AcquireReleaseFileStream}

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/file.h>

typedef doremir_device_file_t    device_t;
typedef doremir_processor_any_t  processor_t;

void test()
{
    device_t    input, output;
    processor_t proc;
    result_t    result;

    proc    = doremir_processor_identity();
    input   = doremir_device_file_open(doremir_str("test/in.wav"));
    output  = doremir_device_file_open(doremir_str("test/out.wav"));

    result = doremir_device_file_run(in, out);

    if (doremir_check(result))
        doremir_print_error(result);

    doremir_device_file_wait(stream);
}
~~~~

### Callback style {#CallbackFileStream}



## Buffer streams {#BufferStream}

~~~~
#include <doremir/time.h>
#include <doremir/thread.h>
#include <doremir/device/file.h>

typedef doremir_device_file_t    device_t;
typedef doremir_processor_any_t  processor_t;

void test()
{
    device_t    input, output;
    processor_t proc;
    result_t    result;

    proc    = doremir_processor_identity();
    input   = doremir_device_buffer_open(doremir_buffer_create(1024));
    output  = doremir_device_buffer_open(doremir_buffer_create(1024));

    result = doremir_device_buffer_run(in, out);

    if (doremir_check(result))
        doremir_print_error(result);

    doremir_device_buffer_wait(stream);
}
~~~~
