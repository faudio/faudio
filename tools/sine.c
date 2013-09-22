
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
typedef fa_signal_t signal_t;

#define N           (44100*10)
#define PI          3.1415
#define TAU         (2 * PI)

#define add_        fa_signal_add
#define mul_        fa_signal_multiply
#define sin_        fa_signal_sin
#define time_       fa_signal_time
#define rand_       fa_signal_random
#define const_      fa_signal_constant
#define imp_        fa_signal_impulse
#define line_       fa_signal_line

void helper_function()
{
    // signal_t a = mul_(sin_(mul_(time_(), const_(TAU * 440))), const_(0.1));
    // signal_t b = mul_(sin_(mul_(time_(), const_(TAU * 440 * 2 / 3))), const_(0.1));
    // signal_t c = mul_(sin_(mul_(time_(), const_(TAU * 440 * 4 / 5))), const_(0.1));
    // signal_t d = mul_(sin_(mul_(time_(), const_(TAU * 440 * 6 / 7))), const_(0.1));
    // signal_t r = add_(add_(a, b), add_(c, d));

    // signal_t r = mul_(rand_(), mul_(sin_(mul_(time_(), const_(TAU * 0.5))), const_(0.5)));
    // signal_t r = mul_(imp_(), const_(0.5));


    // signal_t r = mul_(sin_(line_(440)), const_(0.5));


    double freq = 110;
    double amp = 1;
    signal_t r = const_(0);
    for (int i = 0; i < 100; ++i)
    {
        r = add_(r, mul_(sin_(line_(freq)), const_(amp)));
        freq *= (4.0/3);
        amp  *= 0.9;
    }                    
    r = mul_(r, const_(0.01));



    // double *xs = fa_malloc(8 * N);
    // fa_signal_run(N, r, xs);

    ptr_t res = fa_signal_run_file(N, r, string("test.wav"));

    if (fa_check(res)) {
        fa_error_log(NULL, res);
        exit(-1);
    }

    // fa_signal_print(N,r);
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
