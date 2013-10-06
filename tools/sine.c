
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */
// typedef fa_signal_t signal_t;

#define PI  3.1415
#define TAU (2 * PI)

signal_t fir(ptr_t a, signal_t rec)
{
    return fa_add((signal_t) delay(10, a), fa_multiply(rec, constant(0.9999)));
}
list_t just(ptr_t x, list_t xs)
{
    return x;
}
list_t identity(ptr_t x, list_t xs)
{
    return xs;
}

void helper_function()
{
    signal_t a = fa_multiply(fa_signal_sin(fa_multiply(stime(), constant(TAU * 440))), constant(0.1));
    signal_t b = fa_multiply(fa_signal_sin(fa_multiply(stime(), constant(TAU * 440 * 2 / 3))), constant(0.1));
    signal_t c = fa_multiply(fa_signal_sin(fa_multiply(stime(), constant(TAU * 440 * 4 / 5))), constant(0.1));
    signal_t d = fa_multiply(fa_signal_sin(fa_multiply(stime(), constant(TAU * 440 * 6 / 7))), constant(0.1));
    signal_t r = fa_add(a, fa_add(b, fa_add(c, d)));

    // signal_t r = fa_multiply(random_(), fa_multiply(fa_signal_sin(fa_multiply(stime(), constant(TAU * 0.5))), constant(0.5)));
    // signal_t r = fa_multiply(imp_(), constant(0.5));


    // signal_t r = fa_multiply(fa_signal_sin(line_(440)), constant(0.5));
    // signal_t r = fa_add(constant(0.5), constant(0.5));

    // signal_t r = delay(1, fa_add(constant(0.5), constant(0.5)));
    // signal_t r = fa_signal_input(1);
    // signal_t r = fa_signal_output(1,0,stime());

    // double freq = 100;
    // double amp = 1;
    // signal_t r = constant(0);
    // for (int i = 0; i < 30; ++i) {
    //     r = fa_add(fa_multiply(fa_signal_sin(line_(freq)), constant(amp)), r);
    //     freq *= (5.0 / 4.0);
    //     amp  *= 0.9;
    // }
    // r = fa_multiply(r, constant(0.002));

    // signal_t r = stime();
    // signal_t r = fa_add(
    //     fa_multiply(fa_input(0)                  , fa_signal_sin(line_(0.1))),
    //     fa_multiply(fa_multiply(constant(0.01),random_()) , cos_(line_(0.1)))
    //     );


    // signal_t r = loop_(fir, imp_());


    // double *xs = fa_malloc(8 * N);
    // fa_signal_run(N, r, xs);

    // signal_t r2 = fa_signal_simplify(r);
    // fa_print_ln(fa_signal_draw_tree(fa_signal_to_tree(r2)));


    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);

        // fa_audio_stream_t st = fa_audio_open_stream(i, just, list(
        //     fa_multiply(constant(0.5), fa_multiply(fa_signal_sin(line_(0.2)), fa_input(0)))
        //     ,
        //     fa_multiply(constant(0.5), fa_multiply(cos_(line_(0.2)), fa_input(1)))
        // ), o);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, NULL, NULL);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        while (1) {
            fa_thread_sleep(1000 * 30);
        }

        fa_audio_end_session(s);
    }
    // fa_signal_print(44100*2, r);

    // ptr_t res = fa_signal_run_file(N, r, string("test.wav"));
    // if (fa_check(res)) {
    //     fa_error_log(NULL, res);
    //     exit(-1);
    // }
    // fa_signal_print(N,r);

    r = 0;
}

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();
    helper_function();
    fa_fa_terminate();
}
