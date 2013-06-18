
#ifndef _FAE_MIDI_MESSAGE
#define _FAE_MIDI_MESSAGE

#include <fae/std.h>
#include <fae/pair.h>
#include <fae/buffer.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeMidi Midi
    @{
    @defgroup FaeMidiMessage Message
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
        } fae_midi_message_status_t;
typedef int fae_midi_message_channel_t;
typedef int fae_midi_message_data_t;
typedef struct _fae_midi_message_t * fae_midi_message_t;
fae_midi_message_t fae_midi_message_create_simple(fae_midi_message_status_t,
                                                  int,
                                                  int);
fae_midi_message_t fae_midi_message_create_sysex(fae_buffer_t);
fae_midi_message_t fae_midi_message_copy(fae_midi_message_t);
void fae_midi_message_destroy(fae_midi_message_t);
fae_midi_message_status_t fae_midi_message_status(fae_midi_message_t);
fae_midi_message_channel_t fae_midi_message_channel(fae_midi_message_t);
bool fae_midi_message_is_simple(fae_midi_message_t);
fae_pair_t fae_midi_message_simple_data(fae_midi_message_t);
bool fae_midi_message_is_sysex(fae_midi_message_t);
fae_buffer_t fae_midi_message_sysex_data(fae_midi_message_t);

/** @}
    @}
    @}
    */

#endif // _FAE_MIDI_MESSAGE

