
#ifndef _FAE_MIDI
#define _FAE_MIDI

#include <fae/std.h>
#include <fae/pair.h>
#include <fae/buffer.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeMidi Midi
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
        } fae_midi_status_t;
typedef int fae_midi_channel_t;
typedef int fae_midi_data_t;
typedef struct _fae_midi_t * fae_midi_t;
fae_midi_t fae_midi_create_simple(fae_midi_status_t, int, int);
fae_midi_t fae_midi_create_sysex(fae_buffer_t);
fae_midi_t fae_midi_copy(fae_midi_t);
void fae_midi_destroy(fae_midi_t);
fae_midi_status_t fae_midi_status(fae_midi_t);
fae_midi_channel_t fae_midi_channel(fae_midi_t);
bool fae_midi_is_simple(fae_midi_t);
fae_pair_t fae_midi_simple_data(fae_midi_t);
bool fae_midi_is_sysex(fae_midi_t);
fae_buffer_t fae_midi_sysex_data(fae_midi_t);

/** @}
    @}
    */

#endif // _FAE_MIDI

