
#ifndef _FA_MIDI
#define _FA_MIDI

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/error.h>

/** @addtogroup FaMidi

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
    */

/** A MIDI device.
*/
typedef struct _fa_midi_device_t * fa_midi_device_t;

/** A MIDI session.
*/
typedef struct _fa_midi_session_t * fa_midi_session_t;

/** A MIDI stream.
*/
typedef struct _fa_midi_stream_t * fa_midi_stream_t;

/** A callback to receive MIDI sessions.
*/
typedef fa_midi_session_t (* fa_midi_session_callback_t)(fa_ptr_t,
                                                         fa_midi_session_t);

/** A callback to receive MIDI streams.
*/
typedef fa_midi_stream_t (* fa_midi_stream_callback_t)(fa_ptr_t,
                                                       fa_midi_stream_t);

/** A callback to be invoked upon changes to the MIDI setup.
*/
typedef fa_nullary_t fa_midi_status_callback_t;

/** A callback to be invoked whenever a message is received.
*/
typedef fa_unary_t fa_midi_message_callback_t;

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
    @return
        A device or an error if there are no input devices available.
*/
fa_midi_device_t fa_midi_default_input(fa_midi_session_t);

/** Get the standard output device of the given session.
    @param session   The session.
    @return
        A device or an error if there are no output devices available.
*/
fa_midi_device_t fa_midi_default_output(fa_midi_session_t);

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
void fa_midi_add_status_callback(fa_midi_status_callback_t,
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
void fa_midi_add_message_callback(fa_midi_message_callback_t,
                                  fa_ptr_t,
                                  fa_midi_stream_t);


void fa_midi_set_clock(fa_midi_stream_t, fa_clock_t);

/**
    Schedule an action on the stream.
    @warning Experimental
*/
void fa_midi_schedule(fa_time_t, fa_action_t, fa_midi_stream_t);

/**
    Schedule an action on the stream.
    @warning Experimental
*/
void fa_midi_schedule_relative(fa_time_t,
                               fa_action_t,
                               fa_midi_stream_t);

/** @}
    @}
    */

#endif // _FA_MIDI

