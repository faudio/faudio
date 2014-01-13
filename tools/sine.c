
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
// typedef fa_signal_t signal_t;

#define PI  3.1415
#define TAU (2 * PI)

list_t just(ptr_t x, list_t xs)
{
    return x;
}

void helper_function()
{
    signal_t b = fa_multiply(fa_signal_sin(fa_signal_line(220)), constant(0.1));
    signal_t c = fa_multiply(fa_signal_sin(fa_signal_line(330)), constant(0.1));
    signal_t a = fa_add(b, c);
    // signal_t a = fa_multiply(fa_signal_random(), constant(0.1));

    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out          = list(a, a);

        fa_audio_set_parameter(string("sample-rate"), f64(44100), s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        // while (1) {
        fa_thread_sleep(4 * 1000);
        //   }

        fa_audio_close_stream(st);
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
