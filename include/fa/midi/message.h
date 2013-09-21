
#ifndef _FA_MIDI_MESSAGE
#define _FA_MIDI_MESSAGE

#include <fa/std.h>
#include <fa/pair.h>
#include <fa/buffer.h>

/** @addtogroup FaMidiMessage
 
    @addtogroup FaMidiMessage
    
    Immutable representation of Midi messages.

    @par Literals
    - `midi(note_on, 60, 127)`
    - `midi(note_off, 60, 127)`
    - `midi(0x9, 60, 127)`
    - `midi_sysex(buffer)`

    @par Implements 
    - fa_equal_t
    - fa_order_t
    - fa_string_show_t
    - fa_copy_t
    - fa_destroy_t
    - fa_dynamic_t
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaMidi Midi
    @{
    @defgroup FaMidiMessage Message
    @{
    */


typedef enum {
            note_off,
            note_on,
            after_touch,
            control_change,
            program_change,
            channel_pressure,
            pitch_wheel,
            sysex
        } fa_midi_message_status_t;


typedef int fa_midi_message_channel_t;


typedef int fa_midi_message_data_t;


typedef struct _fa_midi_message_t * fa_midi_message_t;


fa_midi_message_t fa_midi_message_create_simple(fa_midi_message_status_t,
                                                int,
                                                int);


fa_midi_message_t fa_midi_message_create_sysex(fa_buffer_t);


fa_midi_message_t fa_midi_message_copy(fa_midi_message_t);


void fa_midi_message_destroy(fa_midi_message_t);


fa_midi_message_status_t fa_midi_message_status(fa_midi_message_t);


fa_midi_message_channel_t fa_midi_message_channel(fa_midi_message_t);


bool fa_midi_message_is_simple(fa_midi_message_t);


fa_pair_t fa_midi_message_simple_data(fa_midi_message_t);


bool fa_midi_message_is_sysex(fa_midi_message_t);


fa_buffer_t fa_midi_message_sysex_data(fa_midi_message_t);

/** @}
    @}
    @}
    */

#endif // _FA_MIDI_MESSAGE

