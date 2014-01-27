
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>

/*
    This program plays one of more sine waves on the standard audio output device.

 */

#define PI  3.1415
#define TAU (2 * PI)

list_t just(ptr_t x, list_t xs)
{
    return x;
}

// TODO move
#define fa_sizeof_array(A) sizeof(A) / sizeof(A[0])
#define fa_option_show_all(A,S) fa_option_show(fa_sizeof_array(A),A,S)
#define fa_option_parse_all(A,AC,AV) fa_option_parse(fa_sizeof_array(A), A, AC, AV)


fa_option_t options[] = {
    { "f", "frequency",   "Frequency   (default 440)",    fa_option_integral },
    { "d", "duration",    "Duration    (default 5000)",   fa_option_integral },
    { "r", "sample-rate", "Sample rate (default 44100)",  fa_option_integral },
};

void helper_function(int freq1, int freq2, int rate, int duration)
{
    signal_t b = fa_multiply(fa_signal_sin(fa_signal_line(freq1)), constant(0.1));
    signal_t c = fa_multiply(fa_signal_sin(fa_signal_line(freq2)), constant(0.1));
    signal_t a = fa_add(b, c);
    // signal_t a = fa_multiply(fa_signal_random(), constant(0.1));

    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out          = list(a, a);

        fa_audio_set_parameter(string("sample-rate"), f64(rate), s);
        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        if (duration < 0) {
            while (1) {
                fa_thread_sleep(100);
            }
        } else {
            while (duration > 0) {
                duration -= 100;
                fa_thread_sleep(100);
            }
        }

        fa_audio_close_stream(st);
        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    fa_initialize();

    fa_unpair(fa_option_parse_all(options, argc, (char **) argv), opts, _) {
        mark_used(_);
        int freq = fa_map_get(string("frequency"), opts)   ? fa_peek_int32(fa_map_get(string("frequency"), opts)) : 440;
        int rate = fa_map_get(string("sample-rate"), opts) ? fa_peek_int32(fa_map_get(string("sample-rate"), opts)) : 44100;
        int duration = fa_map_get(string("duration"), opts) ? fa_peek_int32(fa_map_get(string("duration"), opts)) : 5000;

        printf("freq=%d, rate=%d, duration=%d\n", freq, rate, duration);
        helper_function(freq, freq * 3 / 2, rate, duration);
    }

    fa_terminate();
}
