
#ifndef _FA_MIDI
#define _FA_MIDI

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/device.h>
#include <fa/action.h>
#include <fa/time.h>
#include <fa/error.h>

/** @addtogroup FaMidi

    @addtogroup FaMidi

    Real-time midi device.

    @par Sessions, devices and streams implement 
    - fa_destroy_t
    - fa_string_show_t

    @par Devices also implement 
    - fa_equal_t

    @par Streams also implement 
    - fa_message_sender_t
    - fa_message_receiver_t
    
    @see 
    - @ref Devices
 
    @defgroup Fa Fa
    @{
    @defgroup FaMidi Midi
    @{
    */


typedef struct _fa_midi_device_t * fa_midi_device_t;


typedef struct _fa_midi_session_t * fa_midi_session_t;


typedef struct _fa_midi_stream_t * fa_midi_stream_t;


typedef fa_midi_session_t (* fa_midi_session_callback_t)(fa_ptr_t,
                                                         fa_midi_session_t);


typedef fa_midi_stream_t (* fa_midi_stream_callback_t)(fa_ptr_t,
                                                       fa_midi_stream_t);


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
void fa_midi_end_session(fa_midi_session_t);

/** Begin a new session, and retain it for the duration of a call to the given function.

    The given function will be called once after the session has created. The session
    will be ended after the callback function has returned. If an error occurs while
    starting the session, the error callback is invoked in place of the session callback.

    @param callback                     Function to receive the sesssion.
    @param error_callback               Function to receive eventual errors.
    @param error_data, session_data     Data closed over by the callbacks.
*/
void fa_midi_with_session(fa_midi_session_callback_t,
                          fa_ptr_t,
                          fa_error_callback_t,
                          fa_ptr_t);

/** Get all active midi devices of the given session.

    @param session   The session.
    @return
        A list of @ref fa_midi_device_t.
*/
fa_list_t fa_midi_all(fa_midi_session_t);

/** Get the standard devices of the given session.

    @param session   The session.
    @return
        A pair of @ref fa_midi_device_t representing the default input and
        output device, respectively.
*/
fa_pair_t fa_midi_default(fa_midi_session_t);

/** Get the standard input device of the given session.
    @param session   The session.
*/
fa_midi_device_t fa_midi_default_input(fa_midi_session_t);

/** Get the standard output device of the given session.
    @param session   The session.
*/
fa_midi_device_t fa_midi_default_output(fa_midi_session_t);

/** Set a callback to be invoked when a status change is detected on the
    given session (mainly useful for hardware setup changes).

    Note that this function will not modify the devices in a session, you have to
    restart the session to get a fresh snapshot.

    @param device   The device.
    @warning
        On OS X this function must be called from the main thread.
*/
void fa_midi_set_status_callback(fa_midi_status_callback_t,
                                 fa_ptr_t,
                                 fa_midi_session_t);

/** Return the name of the given device.
    @param device   The device.
*/
fa_string_t fa_midi_name(fa_midi_device_t);

/** Return the host name of the given device.
    @param device   The device.
*/
fa_string_t fa_midi_host_name(fa_midi_device_t);

/** Return whether the given device has input or not.
    @param device   The device.
*/
bool fa_midi_has_input(fa_midi_device_t);

/** Return whether the given device has output or not.
    @param device   The device.
*/
bool fa_midi_has_output(fa_midi_device_t);

/**
    Open a stream on the given devices.

    @param device   The device.
    @return         A new stream or an error if no stream could be opened.
*/
fa_midi_stream_t fa_midi_open_stream(fa_midi_device_t);

/**
    Close the given stream.
    @param session
        Stream to close.
*/
void fa_midi_close_stream(fa_midi_stream_t);

/**
    Run a stream on the given devices.

    @param device
        The device.
    @param callback
        Function to receive the stream.
    @param error_callback
        Function to receive eventual errors.
*/
void fa_midi_with_stream(fa_midi_device_t,
                         fa_midi_stream_callback_t,
                         fa_ptr_t,
                         fa_error_callback_t,
                         fa_ptr_t);

/**
    Schedule an action on the stream.
    @warning Experimental
*/
void fa_midi_schedule(fa_time_t, fa_action_t, fa_midi_stream_t);

/** @}
    @}
    */

#endif // _FA_MIDI

