
#ifndef _FA_AUDIO_STREAM
#define _FA_AUDIO_STREAM

#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>

/** @addtogroup FaAudioStream

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
    @defgroup FaAudioStream Stream
    @{
    */

/** An audio stream.
*/
typedef struct _fa_audio_stream_t * fa_audio_stream_t;

/** A callback to be invoked whenever a message is received.
*/
typedef fa_unary_t fa_audio_message_callback_t;

/** Return the devices associated with the given stream.
    @param stream   The stream.
    @return A list of @ref fa_audio_device_t
*/
fa_list_t fa_audio_devices(fa_audio_stream_t stream);

/** Return the clock associated with a given stream.
    @param stream The stream.
    @return A clock.
*/
fa_clock_t fa_audio_get_clock(fa_audio_stream_t stream);

/** Return the clock associated with a given stream.
    @param stream The stream.
    @return A clock.
    @deprecated Use @ref fa_audio_get_clock.
*/
fa_clock_t fa_audio_stream_clock(fa_audio_stream_t stream);

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
void fa_audio_add_message_callback(fa_audio_message_callback_t callback,
                                   fa_ptr_t callbackData,
                                   fa_audio_stream_t session);

/**
    Schedule an action on the stream.
    
    The action will be run as soon as the time of the stream (as
    reported by its clock) is greater than or equal to the given due time.
*/
void fa_audio_schedule(fa_time_t time,
                       fa_action_t action,
                       fa_audio_stream_t stream);

/**
    Schedule an action on the stream.

    The action will be run when the given time has passed, relative to
    when this function was invoked. This is a convenience function implemented
    in terms of `fa_audio_schedule` and `fa_clock_time`, using the current
    stream clock.
*/
void fa_audio_schedule_relative(fa_time_t time,
                                fa_action_t action,
                                fa_audio_stream_t stream);

/** @}
    @}
    @}
    */

#endif // _FA_AUDIO_STREAM

