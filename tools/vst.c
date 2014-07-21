
#include <fa/fa.h>
#include <fa/midi/message.h>
#define NO_THREAD_T
#include <fa/util.h>
#undef NO_THREAD_T
#include "../platform/macosx/vst.h"
#include "../shared/signal.h"
#include "common.h"
#include <Carbon/Carbon.h>

/*
    This program implements and plays a trivial custom processor, which generates a click
    sound for each received note. Scheduling is copied from test_dls.
 */

#define RT 1
#define kThisPlugOffset 37 // TODO
// #define PATH fa_string("/Library/Audio/Plug-Ins/VST/Alchemy.vst")
#define PATH fa_string("/Library/Audio/Plug-Ins/VST/ComboV.vst")
// #define PATH fa_string("/Library/Audio/Plug-Ins/VST/Elastik 2.vst")
// #define PATH fa_string("/Library/Audio/Plug-Ins/VST/TAL-U-No-62.vst")
// #define PATH fa_string("/Library/Audio/Plug-Ins/VST/Melodyne.vst")
// #define PATH fa_string("/Library/Audio/Plug-Ins/VST/Kontakt 5.vst")
;


void run_vst()
{
    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_list_t out           = fa_signal_vst(fa_string("dls"), PATH, fa_empty());

        fa_audio_set_parameter(fa_string("sample-rate"), fa_f32(48000), s);
        fa_audio_set_parameter(fa_string("vector-size"), fa_i32(1024), s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }


        fa_audio_schedule_relative(fa_hms(0, 0, 0), fa_action_send(fa_string("dls"), fa_pair_create(fa_string("open"), NULL)), st);

        fa_audio_schedule_relative(fa_hms(0, 0, 0), fa_action_send(fa_string("dls"),
                                                                   fa_midi_message_create_simple(0xc0, 50, 0)), st);

        for (int i = 0; i < 24; ++i) {
            fa_action_t chord = fa_action_send(fa_string("dls"),
                                               fa_midi_message_create_simple(0x90, 52 + ((i % 12) * 2), 90));
            fa_audio_schedule_relative(fa_hms(0, 0, 0), chord, st);
            fa_thread_sleep(150);
        }

        fa_thread_sleep(5000);
        // {
        //     // 201 109 !!!
        //     int j = 200;
        //     int k = 201;
        //
        //     fa_action_t note1  =
        //         fa_action_many(list(
        //             fa_pair_create(fa_action_send(fa_string("midi"), fa_midi_message_create_simple(0x90, 60, 80)), fa_milliseconds(50)),
        //             fa_pair_create(fa_action_send(fa_string("midi"), fa_midi_message_create_simple(0x90, 60, 0)), fa_milliseconds(3))
        //             ));
        //     fa_action_t note2  =
        //         fa_action_many(list(
        //             fa_pair_create(fa_action_send(fa_string("midi"), fa_midi_message_create_simple(0x90, 61, 80)), fa_milliseconds(50)),
        //             fa_pair_create(fa_action_send(fa_string("midi"), fa_midi_message_create_simple(0x90, 61, 0)), fa_milliseconds(3))
        //             ));
        //
        //     fa_action_t notes1 = fa_action_many(fa_list_join(list(
        //                                                          list(
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k - j)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(j))
        //                                                          ),
        //                                                          list(
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k - j)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(j))
        //                                                          ),
        //                                                          list(
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k - j)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(j))
        //                                                          ),
        //                                                          list(
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k - j)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(j))
        //                                                          ),
        //                                                          list(
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(k - j)),
        //                                                              fa_pair_create(note1,             fa_milliseconds(j))
        //                                                          )
        //                                                      )));
        //     fa_action_t notes2 = fa_action_many(list(
        //                                             fa_pair_create(note2,             fa_milliseconds(k * 3)),
        //                                             fa_pair_create(note2,             fa_milliseconds(k * 3)),
        //                                             fa_pair_create(note2,             fa_milliseconds(k * 3)),
        //                                             fa_pair_create(note2,             fa_milliseconds(k * 3)),
        //                                             fa_pair_create(note2,             fa_milliseconds(k * 3))
        //                                         ));
        //
        //     // fa_audio_schedule_relative(seconds(0), notes1, s);
        //     // fa_audio_schedule_relative(seconds(0), notes2, s);
        //
        //     fa_audio_schedule_relative(seconds(0), fa_action_repeat(fa_milliseconds(3000),
        //                                                             fa_action_many(list(
        //                                                                     fa_pair_create(notes1, fa_milliseconds(0)),
        //                                                                     fa_pair_create(notes2, fa_milliseconds(0))
        //                                                                     ))), st);
        //
        //     // 20 BE
        //
        //     // int n = 200;
        //     // fa_audio_schedule_relative(seconds(0), fa_action_repeat(fa_milliseconds(n), note1), s);
        //
        //     fa_thread_sleep(100000);
        // }

    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        run_vst();
    }
}
