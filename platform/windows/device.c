#include <fa/audio.h>
#include <fa/midi.h>
#include <fa/string.h>
#include <fa/thread.h>
#include <fa/util.h>

typedef fa_audio_status_callback_t audio_status_callback_t;
typedef fa_midi_status_callback_t  midi_status_callback_t;

void add_audio_status_listener(audio_status_callback_t function, ptr_t data) {
    assert(false && "implementation missing");
}

void add_midi_status_listener(midi_status_callback_t function, ptr_t data) {
    assert(false && "implementation missing");
}
