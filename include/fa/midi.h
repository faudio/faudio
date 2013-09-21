
#ifndef _FA_MIDI
#define _FA_MIDI

#include <fa/list.h>
#include <fa/pair.h>
#include <fa/device.h>
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


fa_midi_session_t fa_midi_begin_session();


void fa_midi_end_session(fa_midi_session_t);


void fa_midi_with_session(fa_midi_session_callback_t,
                          fa_ptr_t,
                          fa_error_callback_t,
                          fa_ptr_t);


fa_list_t fa_midi_all(fa_midi_session_t);


fa_pair_t fa_midi_default(fa_midi_session_t);


fa_midi_device_t fa_midi_default_input(fa_midi_session_t);


fa_midi_device_t fa_midi_default_output(fa_midi_session_t);


void fa_midi_set_status_callback(fa_midi_status_callback_t,
                                 fa_ptr_t,
                                 fa_midi_session_t);


fa_string_t fa_midi_name(fa_midi_device_t);


fa_string_t fa_midi_host_name(fa_midi_device_t);


bool fa_midi_has_input(fa_midi_device_t);


bool fa_midi_has_output(fa_midi_device_t);


fa_midi_stream_t fa_midi_open_stream(fa_midi_device_t);


void fa_midi_close_stream(fa_midi_stream_t);


void fa_midi_with_stream(fa_midi_device_t,
                         fa_midi_stream_callback_t,
                         fa_ptr_t,
                         fa_error_callback_t,
                         fa_ptr_t);

/** @}
    @}
    */

#endif // _FA_MIDI

