
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program records a 10 second stereo file from the default input device.

 */
#define kRecTime 3
#define kSize (44100*1*8*kRecTime)
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

        signal_t j  = fa_signal_counter();
        // signal_t li = fa_add(fa_multiply(j, constant(2)), constant(0));
        // signal_t ri = fa_add(fa_multiply(j, constant(2)), constant(1));
        // signal_t ls = fa_signal_input(kInputOffset + 0);
        // signal_t rs = fa_signal_input(kInputOffset + 1);

        // signal_t l = fa_signal_record(buf, li, ls);
        // signal_t r = fa_signal_record(buf, ri, rs);
        signal_t mono = fa_signal_record(buf, j, fa_signal_input(kInputOffset));

        // fa_print_ln(l);
        // fa_signal_print(200, li);
        // exit(0);

        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_stream_t st = fa_audio_open_stream(i, 0, just_list, /*list(l, r)*/ list(mono));

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        } else {
            fa_thread_sleep(kRecTime*1000);
        }

        fa_buffer_write_audio(path, buf);

        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    if (argc < 2) {
        fa_print_ln(string("Usage: fa_record FILE"));
    } else {
        helper_function(string((fa_string_utf8_t) argv[1]));
    }

    fa_terminate();
}
