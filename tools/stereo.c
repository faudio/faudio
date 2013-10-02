
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */

list_t just(ptr_t x, list_t inputs)
{
    return x;
}

void helper_function()
{
    signal_t left, right;

    left   = fa_signal_delay(1,       fa_signal_impulses(44100));
    right  = fa_signal_delay(20000+1, fa_signal_impulses(44100));
    // right = fa_signal_delay(22050, fa_signal_impulses(44100));
    
    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
                                                                
        fa_print_ln(list(left, right));
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
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
