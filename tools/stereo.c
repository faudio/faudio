
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program does plays alternating inpulses the left and right speaker.
 */

void play_impulses()
{
    signal_t left, right;

    left   = fa_signal_delay(1,             fa_signal_impulses(44100 / 4));
    right  = fa_signal_delay(1 + 44100 / 8, fa_signal_impulses(44100 / 4));
    // right = fa_signal_delay(22050, fa_signal_impulses(44100));

    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, list(left, right));

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        while (1) {
            fa_thread_sleep(1000 * 30);
        }

        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
#ifdef FAUDIO_DEBUG
    fa_set_log_std();
#endif
    fa_with_faudio() {
        play_impulses();
    }
}
