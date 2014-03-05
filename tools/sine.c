
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>
#include "common.h"

/*
    This program plays one of more sine waves on the standard audio output device.

 */

fa_option_t option_declaration[] = {
    { "a", "amplitude",       "Amplitude   (default 0.1)",    fa_option_floating, "0.1"     },
    { "l", "latency",         "Latency     (default 0.040)",  fa_option_floating, "0.040"   },
    { "f", "frequency",       "Frequency   (default 440)",    fa_option_integral, "410"     },
    { "d", "duration",        "Duration    (default 5000)",   fa_option_integral, "5000"    },
    { "r", "sample-rate",     "Sample rate (default 44100)",  fa_option_integral, "44100"   },
    { "v", "vector-size",     "Vector size (default 64)",     fa_option_integral, "64"      },
    { "n", "number-of-nodes", "Number of nodes (default 1)",  fa_option_integral, "1"       },
};

void run_sines(map_t opts)
{
    const int    number_of_nodes = fa_map_get_int32(string("number-of-nodes"), opts);
    const int    duration        = fa_map_get_int32(string("duration"),        opts);
    const double amplitude       = fa_map_get_double(string("amplitude"),      opts);
    const int    frequency       = fa_map_get_int32(string("frequency"),       opts);

    const int    sample_rate     = fa_map_get_int32(string("sample-rate"),     opts);
    const int    vector_size     = fa_map_get_int32(string("vector-size"),     opts);
    const double latency         = fa_map_get_double(string("latency"),        opts);

    signal_t a = constant(0);

    double f = frequency;
    double x = amplitude;

    for (int i = 0; i < number_of_nodes; ++i) {

        /*
            Build a left-leaning tree, so that the depth is equal to the number of nodes as
            passed on the command line. We could get away with a much larger number
            of nodes by using a balanced tree.
        */

        a = fa_add(fa_multiply(fa_signal_sin(fa_signal_line(f)), constant(x)), a);
        f *= 1.12;
        x *= 0.84;
    }

    // signal_t a = fa_multiply(fa_signal_random(), constant(0.1));

    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out          = list(a, a);

        fa_audio_set_parameter(string("sample-rate"), f64(sample_rate), s);
        fa_audio_set_parameter(string("vector-size"), i32(vector_size), s);
        fa_audio_set_parameter(string("latency"),     f64(latency),     s);

        mark_used(o);
        fa_audio_stream_t st = fa_audio_open_stream(NULL, o, just, out);

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        if (duration < 0) {
            while (1) {
                fa_thread_sleep(10000);
            }
        } else {
            fa_thread_sleep(duration);
        }

        fa_audio_close_stream(st);
        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
#ifdef FAUDIO_DEBUG
    fa_set_log_std();
#endif
    fa_with_faudio() {
        fa_with_options(
            option_declaration, 
            argc, argv, 
            options, args) 
        {
            run_sines(options);
        }        
    }
}
