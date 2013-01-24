
#ifndef _DOREMIR_MIDI
#define _DOREMIR_MIDI

#include <doremir/std.h>
#include <doremir/pair.h>
#include <doremir/buffer.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirMidi Midi
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
        } doremir_midi_status_t;
typedef int doremir_midi_channel_t;
typedef int doremir_midi_data_t;
typedef struct _doremir_midi_t * doremir_midi_t;
doremir_midi_t doremir_midi_create_simple(doremir_midi_status_t,
                                          int,
                                          int);
doremir_midi_t doremir_midi_create_sysex(doremir_buffer_t);
doremir_midi_t doremir_midi_copy(doremir_midi_t);
void doremir_midi_destroy(doremir_midi_t);
doremir_midi_status_t doremir_midi_status(doremir_midi_t);
doremir_midi_channel_t doremir_midi_channel(doremir_midi_t);
bool doremir_midi_is_simple(doremir_midi_t);
doremir_pair_t doremir_midi_simple_data(doremir_midi_t);
bool doremir_midi_is_sysex(doremir_midi_t);
doremir_buffer_t doremir_midi_sysex_data(doremir_midi_t);

/** @}
    @}
    */

#endif // _DOREMIR_MIDI

