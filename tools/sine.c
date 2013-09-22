
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
typedef fa_signal_t signal_t;

#define N (44100*60*60)

void helper_function()
{
    signal_t a = fa_signal_random();

    // double *xs = fa_malloc(8 * N);
    // fa_signal_run(N, a, xs);

    ptr_t res = fa_signal_run_file(N, a, string("test.wav"));
    if (fa_check(res)) {
        fa_error_log(NULL, res);
        exit(-1);
    }

    // fa_signal_print(N,a);
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
