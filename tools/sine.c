
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
typedef fa_signal_t signal_t;

#define N (44100*60)

#define PI 3.1415
#define add_ fa_signal_add
#define mul_ fa_signal_multiply
#define sin_ fa_signal_sin
#define time_ fa_signal_time
#define const_ fa_signal_constant

void helper_function()
{
    signal_t a = mul_(sin_(mul_(time_(), const_(2 * PI * 440))), const_(0.1));
    signal_t b = mul_(sin_(mul_(time_(), const_(2 * PI * 440 * 2 / 3))), const_(0.1));
    signal_t c = mul_(sin_(mul_(time_(), const_(2 * PI * 440 * 4 / 5))), const_(0.1));
    signal_t d = mul_(sin_(mul_(time_(), const_(2 * PI * 440 * 6 / 7))), const_(0.1));


    signal_t r = add_(add_(a, b), add_(c, d));

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
