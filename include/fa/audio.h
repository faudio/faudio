
#ifndef _FA_AUDIO
#define _FA_AUDIO

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/error.h>
#include <fa/signal.h>
#include <fa/action.h>
#include <fa/clock.h>

/** @addtogroup FaAudio

    Provides real-time audio.
    
    These device run processors on the input and output
    the underlying system, typically physical audio interfaces. A running audio
    computation is represented by a *stream*. Access to the current device setups
    is provided by *sessions*.

    @par Implements
    - fa_equal_t
    - fa_destroy_t (sessions and streams)
    - fa_string_show_t

    @see
    - @ref Devices
 
    @defgroup Fa Fa
    @{
    @defgroup FaAudio Audio
    @{
    */

/** An audio session.
*/
typedef struct _fa_audio_session_t * fa_audio_session_t;

/** An audio device.
*/
typedef struct _fa_audio_device_t * fa_audio_device_t;

/** An audio stream.
*/
typedef struct _fa_audio_stream_t * fa_audio_stream_t;

/** A callback to receive audio sessions.
*/
typedef fa_audio_session_t (* fa_audio_session_callback_t)(fa_ptr_t,
                                                           fa_audio_session_t);

/** A callback to receive audio streams.
*/
typedef fa_audio_stream_t (* fa_audio_stream_callback_t)(fa_ptr_t,
                                                         fa_audio_stream_t);

/** A callback to be invoked upon changes to the audio setup.
*/
typedef fa_nullary_t fa_audio_status_callback_t;

/** A callback to be invoked whenever a message is received.
*/
typedef fa_unary_t fa_audio_message_callback_t;

/** An audio processor, or a function from a list of signals to a list of signals.
*/
typedef fa_list_t (* fa_audio_proc_t)(fa_ptr_t, fa_list_t);

/** Begin a new audio session.

     @return
         A new session (errable).
     @par Errors
         Returns an error if the session could not be started.
      
*/
fa_audio_session_t fa_audio_begin_session();

/** End the given session.

    @param session
        Session to end.
*/
void fa_audio_end_session(fa_audio_session_t);

/** Begin a new session, and retain it for the duration of a call to the given function.

    The given function will be called once after the session has created. The session
    will be ended after the callback function has returned. If an error occurs while
    starting the session, the error callback is invoked in place of the session callback.

    @param callback                     Function to receive the sesssion.
    @param error_callback               Function to receive eventual errors.
    @param error_data, session_data     Data closed over by the callbacks.
*/
void fa_audio_with_session(fa_audio_session_callback_t,
                           fa_ptr_t,
                           fa_error_callback_t,
                           fa_ptr_t);

/** Set an audio parameter value.
    
    This function should usually be called once, directly after the session has been
    created, and will affect all streams created after the invocation.

    ### Supported parameters ###
    
    Name           | Description
    ---------------|------------------------------------------------------------------------
    `latency`      | Suggested latency in seconds (integer or floating-point).
    `sample-rate`  | Suggested sample rate (floating-point).
    `vector-size`  | Suggested vector size (integer).
    
    @param name     Name of parameter to set.
    @param value    A [reference](@ref ValueReferences) to the value to set.
    @param session  Session in which to set the parameter.
*/
void fa_audio_set_parameter(fa_string_t,
                            fa_ptr_t,
                            fa_audio_session_t);

/** Get all currently active audio sessions. Note that at most one audio session
    can be active at the same time, so this function returns a list of zero or
    one elements.
    
    @returns A list of @ref fa_audio_session_t.
*/
fa_list_t fa_audio_current_sessions();

/** End all currently active audio sessions.
    
    @returns The null pointer if successful, or an error value otherwise.
*/
fa_ptr_t fa_audio_end_all_sessions();

/** Get all active audio devices of the given session.

    @param session   The session.
    @return
        A list of @ref fa_audio_device_t.
*/
fa_list_t fa_audio_all(fa_audio_session_t);

/** Get the standard devices of the given session.

    @param session   The session.
    @return
        A pair of @ref fa_audio_device_t representing the default input and
        output device, respectively, or an error if at least one of them is not available.
*/
fa_pair_t fa_audio_default(fa_audio_session_t);

/** Get the standard input device of the given session.
    @param session   The session.
    @return
        A device or an error if there are no input devices available.
*/
fa_audio_device_t fa_audio_default_input(fa_audio_session_t);

/** Get the standard output device of the given session.
    @param session   The session.
    @return
        A device or an error if there are no output devices available.
*/
fa_audio_device_t fa_audio_default_output(fa_audio_session_t);

/** Register a callback to be invoked when a hardware change is detected.

    Note that this function will not modify the devices available from a 
    session, you have to start a new session to get a fresh snapshot.
    
    Multiple callbacks can be registered this way. All registered callbacks
    are associated with a session and will be removed when the session ends.

    @param callback
        Callback to register.
    @param callback_data
        Data closed over by the callback function.
    @param session
        Session on which to register the callback.
*/
void fa_audio_add_status_callback(fa_audio_status_callback_t,
                                  fa_ptr_t,
                                  fa_audio_session_t);

/** Return the session associated with the given device.
    @param device   The device.
*/
fa_audio_session_t fa_audio_session(fa_audio_device_t);

/** Return the name of the given device.
    @param device   The device.
*/
fa_string_t fa_audio_name(fa_audio_device_t);

/** Return the host name of the given device.
    @param device   The device.
*/
fa_string_t fa_audio_host_name(fa_audio_device_t);

/** Return whether the given device has input or not.
    @param device   The device.
*/
bool fa_audio_has_input(fa_audio_device_t);

/** Return whether the given device has output or not.
    @param device   The device.
*/
bool fa_audio_has_output(fa_audio_device_t);

/** Return the number of inputs of the given device.
    @param device   The device.
*/
int fa_audio_input_channels(fa_audio_device_t);

/** Return the number of outputs of the given device.
    @param device   The device.
*/
int fa_audio_output_channels(fa_audio_device_t);

/** Return the default sample rate of the given device.
    @param device   The device.
    
*/
double fa_audio_default_sample_rate(fa_audio_device_t);

/**
    Open a stream on the given devices.

    @param input, output    Devices to provide data source and sink.
    @param processor        Processor to run over the devices.
    @return                 A new stream (errable).
    @par Errors
        Returns an error if the session could not be started.
*/
fa_audio_stream_t fa_audio_open_stream(fa_audio_device_t,
                                       fa_audio_device_t,
                                       fa_audio_proc_t,
                                       fa_ptr_t);

/**
    Close the given stream.
    @param session          Stream to close.
*/
void fa_audio_close_stream(fa_audio_stream_t);

/**
    Run a stream on the given devices.

    @param input
        Input device.
    @param input
        Output device.
    @param processor_callback
        Function to receive incoming signals and return output signals.
    @param processor_data
        Pointer passed to processor callback.
    @param stream_callback
        Function to receive the stream if successful.
    @param stream_data
        Pointer passed to stream callback.
    @param error_callback
        Function to errors if unsuccessful.
    @param error_data
        Pointer passed to error callback.
*/
void fa_audio_with_stream(fa_audio_device_t,
                          fa_audio_device_t,
                          fa_audio_proc_t,
                          fa_ptr_t,
                          fa_audio_stream_callback_t,
                          fa_ptr_t,
                          fa_error_callback_t,
                          fa_ptr_t);

/** Return the devices associated with the given stream.
    @param stream   The stream.
    @return A list of @ref fa_audio_device_t
*/
fa_list_t fa_audio_devices(fa_audio_stream_t);

/** Return the clock associated with a given stream.
    @param stream The stream.
    @return A clock.
*/
fa_clock_t fa_audio_get_clock(fa_audio_stream_t);

/** Return the clock associated with a given stream.
    @param stream The stream.
    @return A clock.
    @deprecated Use @ref fa_audio_get_clock.
*/
fa_clock_t fa_audio_stream_clock(fa_audio_stream_t);

/** Register a callback to be invoked when a message is received.

    Multiple callbacks can be registered this way. All registered callbacks
    are associated with a stream and will be removed when the stream is stopped
    or its associated session ends.

    @param callback
        Callback to register.
    @param callback_data
        Data closed over by the callback function.
    @param session
        Stream on which to register the callback.
    @warning
        Experimental.
*/
void fa_audio_add_message_callback(fa_audio_message_callback_t,
                                   fa_ptr_t,
                                   fa_audio_stream_t);

/**
    Schedule an action on the stream.
    
    The action will be run as soon as the time of the stream (as
    reported by its clock) is greater than or equal to the given due time.
*/
void fa_audio_schedule(fa_time_t, fa_action_t, fa_audio_stream_t);

/**
    Schedule an action on the stream.

    The action will be run when the given time has passed, relative to
    when this function was invoked. This is a convenience function implemented
    in terms of `fa_audio_schedule` and `fa_clock_time`, using the current
    stream clock.
*/
void fa_audio_schedule_relative(fa_time_t,
                                fa_action_t,
                                fa_audio_stream_t);

/** @}
    @}
    */

#endif // _FA_AUDIO

