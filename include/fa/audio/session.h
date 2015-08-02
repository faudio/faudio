
#ifndef _FA_AUDIO_SESSION
#define _FA_AUDIO_SESSION

#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/error.h>

/** @addtogroup FaAudioSession

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
    @defgroup FaAudioSession Session
    @{
    */

/** An audio session.
*/
typedef struct _fa_audio_session_t * fa_audio_session_t;

/** A callback to receive audio sessions.
*/
typedef fa_audio_session_t (* fa_audio_session_callback_t)(fa_ptr_t,
                                                           fa_audio_session_t);

/** A callback to be invoked upon changes to the audio setup.
*/
typedef fa_nullary_t fa_audio_status_callback_t;

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
void fa_audio_end_session(fa_audio_session_t session);

/** Begin a new session, and retain it for the duration of a call to the given function.

    The given function will be called once after the session has created. The session
    will be ended after the callback function has returned. If an error occurs while
    starting the session, the error callback is invoked in place of the session callback.

    @param callback                     Function to receive the sesssion.
    @param error_callback               Function to receive eventual errors.
    @param error_data, session_data     Data closed over by the callbacks.
*/
void fa_audio_with_session(fa_audio_session_callback_t sessionCallback,
                           fa_ptr_t ptr,
                           fa_error_callback_t callback,
                           fa_ptr_t ptr_);

/** Set an audio parameter value.
    
    This function should usually be called once, directly after the session has been
    created, and will affect all streams created after the invocation.
    
    Note the dependency between sample-rate, latency and vector size: Normally you want
    `(1/sr)*vs < min(inputLatency, outputLatency)` to hold.

    ### Supported parameters ###
    
    Name                 | Description
    ---------------------|------------------------------------------------------------------------
    `latency`            | Suggested latency for both input and output in seconds (integer or floating-point).
    `input-latency`      | Suggested latency for both input and output in seconds (integer or floating-point).
    `output-latency`     | Suggested latency for both input and output in seconds (integer or floating-point).
    `sample-rate`        | Suggested sample rate in Hertz (floating-point).
    `vector-size`        | Suggested internal vector size (integer).
    `scheduler-interval` | Scheduler update interval in milliseconds (integer).
    `exclusive`          | Use exclusive mode if available (boolean or integer). This will force `faudio` to claim exclusive access to each audio device it uses.
    
    @param name     Name of parameter to set.
    @param value    A [reference](@ref ValueReferences) to the value to set.
    @param session  Session in which to set the parameter.
*/
void fa_audio_set_parameter(fa_string_t string,
                            fa_ptr_t ptr,
                            fa_audio_session_t session);

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
fa_list_t fa_audio_all(fa_audio_session_t session);

/** @}
    @}
    @}
    */

#endif // _FA_AUDIO_SESSION

