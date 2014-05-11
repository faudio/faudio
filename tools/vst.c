
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
#define PATH string("/Library/Audio/Plug-Ins/VST/ComboV.vst")
// #define PATH string("/Library/Audio/Plug-Ins/VST/TAL-U-No-62.vst")
// #define PATH string("/Library/Audio/Plug-Ins/VST/Melodyne.vst")
// #define PATH string("/Library/Audio/Plug-Ins/VST/Kontakt 5.vst")
;


void run_vst()
{
    if (RT) {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out           = fa_signal_vst(string("vst-test"), PATH, empty());

        fa_audio_set_parameter(string("sample-rate"), f32(48000), s);
        fa_audio_set_parameter(string("vector-size"), i32(1024), s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        for (int i = 0; i < 24; ++i) {           
            fa_action_t chord = fa_action_send(string("vst-test"), fa_midi_message_create_simple(0x90, 52 + ((i % 12) * 5), 90));
            fa_audio_schedule_relative(hms(0, 0, 0), chord, st);
            fa_thread_sleep(150);
        }
        // fa_thread_sleep(5000);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        run_vst();
    }
}
