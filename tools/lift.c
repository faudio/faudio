
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
// typedef fa_signal_t signal_t;

#define N           (44100*60)
#define PI          3.1415
#define TAU         (2 * PI)

#define add_        fa_signal_add
#define mul_        fa_signal_multiply
#define sin_        fa_signal_sin
#define cos_        fa_signal_cos
#define time_       fa_signal_time
#define random_     fa_signal_random
#define const_      fa_signal_constant
#define imp_        fa_signal_impulse
#define line_       fa_signal_line
#define delay_      fa_signal_delay
#define loop_       fa_signal_loop
#define input_      fa_signal_input
#define output_     fa_signal_output

list_t just(ptr_t x, list_t xs)
{
    return x;
}

signal_t fir(ptr_t a, signal_t rec)
{
    return add_((signal_t) delay_(10, a), mul_(rec, const_(0.9999)));
}

double times (ptr_t _, double x, double y)
{
    return x + y;
}

void helper_function()
{
    signal_t r = fa_signal_lift2(string("times"), times, NULL, const_(0.1), sin_(line_(440)));
    
    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, list(r,r));

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
