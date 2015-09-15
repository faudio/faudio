
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


typedef unsigned char fa_midi_message_status_t;

#define msg_note_off           0x80
#define msg_note_on            0x90
#define msg_after_touch        0xA0
#define msg_control_change     0xB0
#define msg_program_change     0xC0
#define msg_channel_pressure   0xD0
#define msg_pitch_wheel        0xE0
#define msg_sysex              0xF0

typedef int fa_midi_message_channel_t;


typedef int fa_midi_message_data_t;

/** A MIDI message, which is either *simple* or *system exclusive*.
*/
typedef struct _fa_midi_message_t * fa_midi_message_t;

/** Creates a simple message from the given components.
    @param status   The status byte.
    @param data1    The first data byte.
    @param data2    The second data byte.
    @return         A new Midi message.
*/
fa_midi_message_t fa_midi_message_create_simple(fa_midi_message_status_t status,
                                                int int_,
                                                int int__);

/** Creates a sysex message from the given data buffer (not including F0 and F7).
    @param data     Raw data buffer (transfered).
    @return         A new sysex message.
*/
fa_midi_message_t fa_midi_message_create_sysex(fa_buffer_t buffer);

/** Copy the given midi message.
*/
fa_midi_message_t fa_midi_message_copy(fa_midi_message_t message);

/** Destroy the given midi_message message.
*/
void fa_midi_message_destroy(fa_midi_message_t message);

/** Return whether the given midi_message message is a simple message.

        fa_midi_message_is_sysex(a) == !fa_midi_message_is_simple(x)
*/
bool fa_midi_message_is_simple(fa_midi_message_t message);

/** Returns the status and channel part of a MIDI message.

    @warning
        Fails if the given message is not simple.
        Should only be used in conjunction with fa_midi_message_is_simple.
*/
fa_pair_t fa_midi_message_simple_data(fa_midi_message_t message);

/** Return the operation part of the status byte of given MIDI message.

    @warning
        Fails if the given message is not simple.
        Should only be used in conjunction with fa_midi_message_is_simple.
*/
fa_midi_message_status_t fa_midi_message_status(fa_midi_message_t message);

/** Return the channel part of the status byte of a given MIDI message.

    @warning
        Fails if the given message is not simple.
        Should only be used in conjunction with fa_midi_message_is_simple.
*/
fa_midi_message_channel_t fa_midi_message_channel(fa_midi_message_t message);

/** Return whether the given MIDI message is a sysex message.

        fa_midi_message_is_sysex(a) == !fa_midi_message_is_simple(x)
*/
bool fa_midi_message_is_sysex(fa_midi_message_t message);

/** Return the data buffer of a sysex message, not including the surrounding `F0` and `F7` bytes.

    @warning
        Fails if the given message is not a sysex message.
        Should only be used in conjunction with fa_midi_message_is_sysex.
*/
fa_buffer_t fa_midi_message_sysex_data(fa_midi_message_t message);


void fa_midi_message_decons(fa_midi_message_t midi_message, uint8_t *statusCh, uint8_t *data1, uint8_t *data2);



/** @}
    @}
    @}
    */

#endif // _FA_MIDI_MESSAGE

