
#ifndef _FA_AUDIO_DEVICE
#define _FA_AUDIO_DEVICE

#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/audio/session.h>

/** @addtogroup FaAudioDevice

    Provides real-time audio.
    
    These device run processors on the input and output
    the underlying system, typically physical audio interfaces. A running audio
    computation is represented by a *stream*. Access to the current device setups
    is provided by *sessions*.

    @par Implements
    - fa_equal_ta
    - fa_destroy_t (sessions and streams)
    - fa_string_show_t

    @see
    - @ref Devices
 
    @defgroup Fa Fa
    @{
    @defgroup FaAudio Audio
    @{
    @defgroup FaAudioDevice Device
    @{
    */

/** An audio device.
*/
typedef struct _fa_audio_device_t * fa_audio_device_t;

/** An audio processor, or a function from a list of signals to a list of signals.
*/
typedef fa_list_t (* fa_audio_proc_t)(fa_ptr_t, fa_list_t);

/** Get the standard devices of the given session.

    @param session   The session.
    @return
        A pair of @ref fa_audio_device_t representing the default input and
        output device, respectively, or an error if at least one of them is not available.
*/
fa_pair_t fa_audio_default(fa_audio_session_t session);

/** Get the standard input device of the given session.
    @param session   The session.
    @return
        A device or an error if there are no input devices available.
*/
fa_audio_device_t fa_audio_default_input(fa_audio_session_t session);

/** Get the standard output device of the given session.
    @param session   The session.
    @return
        A device or an error if there are no output devices available.
*/
fa_audio_device_t fa_audio_default_output(fa_audio_session_t session);

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
void fa_audio_add_status_callback(fa_audio_status_callback_t statusCallback,
                                  fa_ptr_t ptr,
                                  fa_audio_session_t session);

/** Return the session associated with the given device.
    @param device   The device.
*/
fa_audio_session_t fa_audio_session(fa_audio_device_t device);

/** Return the name of the given device.
    @param device   The device.
*/
fa_string_t fa_audio_name(fa_audio_device_t device);

/** Return the host name of the given device.
    @param device   The device.
*/
fa_string_t fa_audio_host_name(fa_audio_device_t device);

/** Return whether the given device has input or not.
    @param device   The device.
*/
bool fa_audio_has_input(fa_audio_device_t device);

/** Return whether the given device has output or not.
    @param device   The device.
*/
bool fa_audio_has_output(fa_audio_device_t device);

/** Return the number of inputs of the given device.
    @param device   The device.
*/
int fa_audio_input_channels(fa_audio_device_t device);

/** Return the number of outputs of the given device.
    @param device   The device.
*/
int fa_audio_output_channels(fa_audio_device_t device);

/** Return the current sample rate of the given device.
    @param device   The device.
    
*/
double fa_audio_current_sample_rate(fa_audio_device_t device);

/** Return the default sample rate of the given device.
    @param device   The device.
    @deprecated 
        This function was incorrectly named, it refers to
        sample rate at the time of the device snapshot (i.e.
        when the session starts), not to its default setting.
        
        Use @ref fa_audio_current_sample_rate instead.
    
*/
double fa_audio_default_sample_rate(fa_audio_device_t device);

/** @}
    @}
    @}
    */

#endif // _FA_AUDIO_DEVICE

