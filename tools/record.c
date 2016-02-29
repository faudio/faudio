
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program records a 10 second stereo file from the default input device.

 */
#define kRecTime 5
#define kSize (44100*2*8*kRecTime)
#define kInputOffset 8 // FIXME

void helper_function(fa_string_t path)
{
    {
        fa_buffer_t buf = fa_buffer_create(kSize);

        if (fa_error_check(buf)) {
            fa_print("Error: Could not read file '%s'\n", path);
            exit(-1);
        }

        bool mono = false; // TODO

        fa_signal_t j  = fa_signal_counter();
        fa_list_t signals = NULL;

        if (mono) {
            fa_signal_t x = fa_signal_record(buf, j, fa_signal_input(kInputOffset));
            signals = list(x);
        } else {
            fa_signal_t li = fa_add(fa_multiply(j, fa_constant(2)), fa_constant(0));
            fa_signal_t ri = fa_add(fa_multiply(j, fa_constant(2)), fa_constant(1));
            fa_signal_t x = fa_signal_input(kInputOffset + 0);

            fa_signal_t l = fa_signal_record(buf, li, x);
            fa_signal_t r = fa_signal_record(buf, ri, x);
            signals = list(l, r);
        }

        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_stream_t st = fa_audio_open_stream(i, NULL, just_list, signals);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        } else {
            fa_thread_sleep(kRecTime * 1000);
        }

        fa_buffer_set_meta(buf, fa_string("channels"),    fa_i32(2));
        fa_buffer_set_meta(buf, fa_string("generator"),   fa_string("faudio"));
        fa_buffer_set_meta(buf, fa_string("sample-rate"), fa_f64(44100));
        fa_buffer_write_audio(path, buf);
        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        if (argc < 2) {
            fa_print_ln(fa_string("Usage: fa_record [file]"));
        } else {
            helper_function(fa_string(argv[1]));
        }
    }
}
