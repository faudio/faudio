
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program plays a couple of notes on the standard audio output device using
    internal synth (OS X only).
 */


#define RT 1

void run_dls()
{
#ifndef _WIN32
    fa_pair_t synth = fa_signal_dls();
    string_t name   = string("dls");
#else
    fa_pair_t synth = fa_signal_synth(string("C:\\sf.sf2"));
    string_t name   = string("fluid");
#endif

    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        // fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);

        list_t out           = fa_pair_to_list(synth);

        fa_audio_stream_t st = fa_audio_open_stream(NULL, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        for (int i = 0; i < 10; ++i) {

            // fa_clock_t cl = fa_clock_standard();
            // fa_clock_t cl = fa_audio_stream_clock(st);
            // mark_used(cl);

            // printf("Scheduling msec: %lld \n", fa_clock_milliseconds(cl));
            // printf("Scheduling time: %s \n", unstring(fa_string_show(fa_clock_time(cl))));

            fa_action_t chord = fa_action_many(list(
                                                   fa_pair_create(
                                                       fa_action_send(name, fa_midi_message_create_simple(0x90, 64 + ((i % 12) * 3), 90)),
                                                       hms(0, 0, 0)
                                                   ),
                                                   fa_pair_create(
                                                       fa_action_send(name, fa_midi_message_create_simple(0x90, 60 + ((i % 12) * 3), 90)),
                                                       hms(0, 0, 0)
                                                   )
                                               ));
            // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
            fa_audio_schedule_relative(
                hms(0, 0, 0),
                chord,
                st);
            fa_thread_sleep(150);
        }

        fa_destroy(st);
        fa_destroy(s);
    } else {
        fa_signal_run_file(44100 * 60, list(
                               fa_pair_create(
                                   hms(0, 0, 0),
                                   fa_action_send(name, fa_midi_message_create_simple(0x90, 60 + ((0 % 12) * 3), 90))
                               ),
                               fa_pair_create(
                                   hms(0, 0, 1),
                                   fa_action_send(name, fa_midi_message_create_simple(0x90, 60 + ((1 % 12) * 3), 90))
                               ),
                               fa_pair_create(
                                   hms(0, 0, 2),
                                   fa_action_send(name, fa_midi_message_create_simple(0x90, 60 + ((2 % 12) * 3), 90))
                               )

                           ),
                           fa_pair_first(fa_signal_dls()),
                           string("test.wav"));
    }

}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        run_dls();
    }
}
