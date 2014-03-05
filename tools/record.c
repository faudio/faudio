
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program records a 10 second stereo file from the default input device.

 */
#define kRecTime 5
#define kSize (44100*2*8*kRecTime)
#define kInputOffset 8 // FIXME

list_t just_list(ptr_t x, list_t xs)
{
    return x;
}

signal_t add1(ptr_t _, signal_t x)
{
    return fa_add(x, constant(1));
}


void helper_function(string_t path)
{
    {
        buffer_t buf = fa_buffer_create(kSize);

        if (fa_error_check(buf)) {
            fa_print("Error: Could not read file '%s'\n", path);
            exit(-1);
        }

        bool mono = false; // TODO

        signal_t j  = fa_signal_counter();
        list_t signals = NULL;

        if (mono) {
            signal_t x = fa_signal_record(buf, j, fa_signal_input(kInputOffset));
            signals = list(x);
        } else {
            signal_t li = fa_add(fa_multiply(j, constant(2)), constant(0));
            signal_t ri = fa_add(fa_multiply(j, constant(2)), constant(1));
            signal_t x = fa_signal_input(kInputOffset + 0);

            signal_t l = fa_signal_record(buf, li, x);
            signal_t r = fa_signal_record(buf, ri, x);
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

        fa_buffer_write_audio(path, buf);

        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        if (argc < 2) {
            fa_print_ln(string("Usage: fa_record [file]"));
        } else {
            helper_function(string((fa_string_utf8_t) argv[1]));
        }
    }
}
