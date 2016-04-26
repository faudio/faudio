
#ifndef _FA_AUDIO_STREAM
#define _FA_AUDIO_STREAM

#include <fa/action.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/list.h>
#include <fa/pair.h>
#include <fa/map.h>
#include <fa/error.h>
#include <fa/signal.h>
#include <fa/action.h>
#include <fa/clock.h>
#include <fa/audio/device.h>

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
    
    Received a `(Name, Msg)` pair, where `Name` is `Action.Name` and `Msg` a
    copyable, showable value. 
*/
typedef fa_binary_t fa_audio_message_callback_t;

/** A callback to be invoked when an audio stream is closed.

    Receives a Ptr with the user_data value passed to
    fa_open_stream_with_callbacks, and a bool, telling
    if the stream was closed normally.
*/
typedef void (* fa_audio_stream_closed_callback_t)(fa_ptr_t, bool);

/** A callback to receive audio streams.
*/
typedef fa_audio_stream_t (* fa_audio_stream_callback_t)(fa_ptr_t,
                                                         fa_audio_stream_t);

/**
    Open a stream on the given devices.

    @param input, output    Devices to provide data source and sink.
    @param processor        Processor to run over the devices.
    @return                 A new stream (errable).
    @par Errors
        Returns an error if the session could not be started.
*/
fa_audio_stream_t fa_audio_open_stream(fa_audio_device_t device,
                                       fa_audio_device_t device_,
                                       fa_audio_proc_t proc,
                                       fa_ptr_t ptr);
                                       
/**
    Open a stream on the given devices and attach callbacks

    Just as fa_audio_open_stream, but attaches a message callback
    and a closed callback.
*/
fa_audio_stream_t fa_audio_open_stream_with_callbacks(fa_audio_device_t device,
                                                      fa_audio_device_t device_,
                                                      fa_audio_proc_t proc,
                                                      fa_ptr_t ptr,
                                                      fa_audio_message_callback_t message_callback,
                                                      fa_audio_stream_closed_callback_t closed_callback,
                                                      fa_ptr_t user_data);

/**
    Close the given stream.
    @param session          Stream to close.
*/
void fa_audio_close_stream(fa_audio_stream_t stream);

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
void fa_audio_with_stream(fa_audio_device_t device,
                          fa_audio_device_t device_,
                          fa_audio_proc_t proc,
                          fa_ptr_t ptr,
                          fa_audio_stream_callback_t streamCallback,
                          fa_ptr_t ptr_,
                          fa_error_callback_t callback,
                          fa_ptr_t ptr__);

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

/** Set the speed of the virtual time for the given streem.

    By default, time runs at speed `1.0`, meaning that the stream clock returns the actual processing time.
    More generally, the time returned by an audio stream clock is the integral of its speed over time.
    
    @note
        This only affects scheduling, audio processing always run at a fixed sample rate.

    @param speed
        Speed to set.
    @param stream
        The stream.
      
*/
void fa_audio_set_speed(double speed, fa_audio_stream_t stream);

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
*/
void fa_audio_add_message_callback(fa_audio_message_callback_t callback,
                                   fa_ptr_t callbackData,
                                   fa_audio_stream_t stream);

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
void fa_audio_schedule_now(fa_action_t action,
                           fa_audio_stream_t stream);

/**
    Return the sample rate of the given stream.
    @param stream   The stream.
*/
double fa_audio_stream_sample_rate(fa_audio_stream_t stream);

/**
    Return info of the stream, in form of a map.

    The map should be destroyed by the caller.
*/
fa_map_t fa_audio_stream_get_info(fa_audio_stream_t stream);

/** @}
    @}
    @}
    */

#endif // _FA_AUDIO_STREAM

