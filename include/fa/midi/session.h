
#ifndef _FA_MIDI_SESSION
#define _FA_MIDI_SESSION

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/error.h>

/** @addtogroup FaMidiSession

    Provides real-time MIDI.

    @par Sessions, devices and streams implement 
    - fa_destroy_t
    - fa_string_show_t

    @par Devices also implement 
    - fa_equal_t

    @see 
    - @ref Devices
 
    @defgroup Fa Fa
    @{
    @defgroup FaMidi Midi
    @{
    @defgroup FaMidiSession Session
    @{
    */

/** A MIDI session.
*/
typedef struct _fa_midi_session_t * fa_midi_session_t;

/** A callback to receive MIDI sessions.
*/
typedef fa_midi_session_t (* fa_midi_session_callback_t)(fa_ptr_t,
                                                         fa_midi_session_t);

/** A callback to be invoked upon changes to the MIDI setup.
*/
typedef fa_nullary_t fa_midi_status_callback_t;

/** Begin a new midi session.

    @return
        A new session.
    @par Errors
        Fails if the session could not be started.
*/
fa_midi_session_t fa_midi_begin_session();

/** End the given session.

    @param session
        Session to end.
*/
void fa_midi_end_session(fa_midi_session_t session);

/** Begin a new session, and retain it for the duration of a call to the given function.

    The given function will be called once after the session has created. The session
    will be ended after the callback function has returned. If an error occurs while
    starting the session, the error callback is invoked in place of the session callback.

    @param callback                     Function to receive the sesssion.
    @param error_callback               Function to receive eventual errors.
    @param error_data, session_data     Data closed over by the callbacks.
*/
void fa_midi_with_session(fa_midi_session_callback_t sessionCallback,
                          fa_ptr_t ptr,
                          fa_error_callback_t callback,
                          fa_ptr_t ptr_);

/** Get all currently active MIDI sessions. Note that at most one midi session
    can be active at the same time, so this function returns a list of zero or
    one elements.
    
    @returns A list of @ref fa_midi_session_t.
*/
fa_list_t fa_midi_current_sessions();

/** End all currently active MIDI sessions.
    
    @returns The null pointer if successful, or an error value otherwise.
*/
fa_ptr_t fa_midi_end_all_sessions();

/** Get all active midi devices of the given session.

    @param session   The session.
    @return
        A list of @ref fa_midi_device_t.
*/
fa_list_t fa_midi_all(fa_midi_session_t session);

/** Get the standard devices of the given session.

    @param session   The session.
    @return
        A pair of @ref fa_midi_device_t representing the default input and
        output device, respectively, or an error if at least one of them is not available.
*/
fa_pair_t fa_midi_default(fa_midi_session_t session);

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
    @warning
        Experimental.
*/
void fa_midi_add_status_callback(fa_midi_status_callback_t statusCallback,
                                 fa_ptr_t ptr,
                                 fa_midi_session_t session);

/** @}
    @}
    @}
    */

#endif // _FA_MIDI_SESSION

