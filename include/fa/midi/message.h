
#ifndef _FA_MIDI_MESSAGE
#define _FA_MIDI_MESSAGE

#include <fa/std.h>
#include <fa/pair.h>
#include <fa/buffer.h>

/** @addtogroup FaMidiMessage
 
    Immutable MIDI messages.

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

/** Creates a simple message from the given components.
    @param status   The status byte.
    @param data1    The first data byte.
    @param data2    The second data byte.
    @return         A new Midi message.
*/
fa_midi_message_t fa_midi_message_create_simple(fa_midi_message_status_t,
                                                int,
                                                int);

/** Creates a sysex message from the given data buffer (not including F0 and F7).
    @param data     Raw data buffer (transfered).
    @return         A new sysex message.
*/
fa_midi_message_t fa_midi_message_create_sysex(fa_buffer_t);

/** Copy the given midi message.
*/
fa_midi_message_t fa_midi_message_copy(fa_midi_message_t);

/** Destroy the given midi_message message.
*/
void fa_midi_message_destroy(fa_midi_message_t);

/** Return whether the given midi_message message is a simple message.
*/
bool fa_midi_message_is_simple(fa_midi_message_t);

/** Returns the status and channel part of a MIDI message.
*/
fa_pair_t fa_midi_message_simple_data(fa_midi_message_t);

/** Return the status byte of given MIDI message.
*/
fa_midi_message_status_t fa_midi_message_status(fa_midi_message_t);

/** Return the channel byte of given MIDI message.
*/
fa_midi_message_channel_t fa_midi_message_channel(fa_midi_message_t);

/** Return whether the given MIDI message is a sysex message.
*/
bool fa_midi_message_is_sysex(fa_midi_message_t);

/** Return the data buffer of a sysex message, except for the wrapping `F0` and `F7` bytes.
*/
fa_buffer_t fa_midi_message_sysex_data(fa_midi_message_t);

/** @}
    @}
    @}
    */

#endif // _FA_MIDI_MESSAGE

