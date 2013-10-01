
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


void helper_function()
{
    signal_t r = mul_(input_(32), random_());

    fa_signal_run_file(44100*10, r, string("test.wav"));
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
