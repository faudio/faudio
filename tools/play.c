
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program does plays the given audio file in the default output device.

 */
// typedef fa_signal_t signal_t;

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
        buffer_t buf = fa_buffer_read_audio(path);

        if (fa_error_check(buf)) {
            fa_print("Error: Could not read file '%s'\n", path);
            exit(-1);
        }

        signal_t j  = fa_signal_counter();
        signal_t li = fa_add(fa_multiply(j, constant(2)), constant(0));
        signal_t ri = fa_add(fa_multiply(j, constant(2)), constant(1));

        signal_t l = fa_signal_play(buf, li);
        signal_t r = fa_signal_play(buf, ri);

        // fa_print_ln(l);
        // fa_signal_print(200, li);
        // exit(0);

        fa_audio_session_t s = fa_audio_begin_session();
        // fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_audio_stream_t st = fa_audio_open_stream(0, o, just_list, list(l, r));

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        } else {
            while (1) {
                fa_thread_sleep(10000);
            }
        }

        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        if (argc < 2) {
            fa_print_ln(string("Usage: fa_play [file]"));
        } else {
            helper_function(string((fa_string_utf8_t) argv[1]));
        }
    }
}
