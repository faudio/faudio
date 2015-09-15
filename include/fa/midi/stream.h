
#ifndef _FA_MIDI_STREAM
#define _FA_MIDI_STREAM

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/error.h>
#include <fa/midi/device.h>

/** @addtogroup FaMidiStream

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
    @defgroup FaMidiStream Stream
    @{
    */

/** A MIDI stream.
*/
typedef struct _fa_midi_stream_t * fa_midi_stream_t;

/** A callback to receive MIDI streams.
*/
typedef fa_midi_stream_t (* fa_midi_stream_callback_t)(fa_ptr_t,
                                                       fa_midi_stream_t);

/** A callback to be invoked whenever a message is received.
*/
typedef fa_unary_t fa_midi_message_callback_t;

/**
    Open a stream on the given devices.

    @param device   The device.
    @return         A new stream or an error if no stream could be opened.
*/
fa_midi_stream_t fa_midi_open_stream(fa_midi_device_t device);

/**
    Close the given stream.
    @param session
        Stream to close.
*/
void fa_midi_close_stream(fa_midi_stream_t stream);

/**
    Run a stream on the given devices.

    @param device
        The device.
    @param callback
        Function to receive the stream.
    @param error_callback
        Function to receive eventual errors.
*/
void fa_midi_with_stream(fa_midi_device_t device,
                         fa_midi_stream_callback_t streamCallback,
                         fa_ptr_t ptr,
                         fa_error_callback_t callback,
                         fa_ptr_t ptr_);

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
void fa_midi_add_message_callback(fa_midi_message_callback_t messageCallback,
                                  fa_ptr_t ptr,
                                  fa_midi_stream_t stream);

/** Associate the given clock with the given stream.
    @param stream The stream.
    @param The clock.
*/
void fa_midi_set_clock(fa_midi_stream_t stream, fa_clock_t clock);

/** Return the clock associated with a given stream.
    @param stream The stream.
    @return A clock.
*/
fa_clock_t fa_midi_get_clock(fa_midi_stream_t stream);

/**
    Schedule an action on the stream.
    
    The action will be run as soon as the time of the stream (as
    reported by its clock) is greater than or equal to the given due time.
*/
void fa_midi_schedule(fa_time_t time,
                      fa_action_t action,
                      fa_midi_stream_t stream);

/**
    Schedule an action on the stream.

    The action will be run when the given time has passed, relative to
    when this function was invoked. This is a convenience function implemented
    in terms of `fa_audio_schedule` and `fa_clock_time`, using the current
    stream clock.
*/
void fa_midi_schedule_relative(fa_time_t time,
                               fa_action_t action,
                               fa_midi_stream_t stream);

/**
    Schedule an action to be run immediately.

    Can be used with non-compound, non-do actions.
    As a special case, non-nested compound actions can also
    be passed, note however that the contained actions
    will all be executed immediately, regardless of their
    specified timing. The order is still guaranteed though.

    Note also that actions scheduled this way may be run before actions
    scheduled using fa_schedule_relative(fa_now(), ...)
 */
void fa_midi_schedule_now(fa_action_t action,
                          fa_midi_stream_t stream);

/** @}
    @}
    @}
    */

#endif // _FA_MIDI_STREAM

