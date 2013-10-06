
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
// typedef fa_signal_t signal_t;

list_t just(ptr_t x, list_t xs)
{
    return x;
}

signal_t fir(ptr_t a, signal_t rec)
{
    return fa_add((signal_t) delay(10, a), fa_multiply(rec, constant(0.9999)));
}

double times(ptr_t _, double x, double y)
{
    return x + y;
}

void helper_function()
{
    signal_t r = fa_signal_lift2(string("times"), times, NULL, constant(0.1), fa_signal_sin(line(440)));

    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, list(r, r));

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
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
