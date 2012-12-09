
#ifndef _DOREMIR_MIDI
#define _DOREMIR_MIDI

#include <Doremir/Buffer.h>

/** @defgroup Doremir
    @{
    @defgroup Midi
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
int doremir_midi_status_type(int);
int doremir_midi_status_channel(int);
bool doremir_midi_status_is_sysex(int);
typedef struct {
            uint8_t bytes[3];
        } doremir_midi_simple_message_t;
typedef struct {
            doremir_buffer_t data;
        } doremir_midi_sysex_message_t;
typedef enum {
            simple_message_tag, sysex_message_tag
        } doremir_midi_message_tag_t;
typedef struct {
            doremir_midi_message_tag_t tag;
            union {
                doremir_midi_simple_message_t simple;
                doremir_midi_sysex_message_t sysex;
            } value;
        } doremir_midi_message_t;

/** @}
    @}
    */

#endif // _DOREMIR_MIDI

