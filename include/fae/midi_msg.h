
#ifndef _FAE_MIDIMSG
#define _FAE_MIDIMSG

#include <fae/std.h>
#include <fae/pair.h>
#include <fae/buffer.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeMidiMsg MidiMsg
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
        } fae_midi_msg_status_t;
typedef int fae_midi_msg_channel_t;
typedef int fae_midi_msg_data_t;
typedef struct _fae_midi_msg_t * fae_midi_msg_t;
fae_midi_msg_t fae_midi_msg_create_simple(fae_midi_msg_status_t,
                                          int,
                                          int);
fae_midi_msg_t fae_midi_msg_create_sysex(fae_buffer_t);
fae_midi_msg_t fae_midi_msg_copy(fae_midi_msg_t);
void fae_midi_msg_destroy(fae_midi_msg_t);
fae_midi_msg_status_t fae_midi_msg_status(fae_midi_msg_t);
fae_midi_msg_channel_t fae_midi_msg_channel(fae_midi_msg_t);
bool fae_midi_msg_is_simple(fae_midi_msg_t);
fae_pair_t fae_midi_msg_simple_data(fae_midi_msg_t);
bool fae_midi_msg_is_sysex(fae_midi_msg_t);
fae_buffer_t fae_midi_msg_sysex_data(fae_midi_msg_t);

/** @}
    @}
    */

#endif // _FAE_MIDIMSG

