
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>

/*
    This program plays one of more sine waves on the standard audio output device.

 */

list_t just(ptr_t x, list_t xs)
{
    return x;
}

// TODO move
#define fa_map_get_the(TYPE, KEY, MAP) \
    (fa_peek_##TYPE(fa_map_get(KEY, MAP)))

#define fa_map_get_int32(KEY, MAP) fa_map_get_the(int32, KEY, MAP)
#define fa_map_get_int64(KEY, MAP) fa_map_get_the(int64, KEY, MAP)
#define fa_map_get_double(KEY, MAP) fa_map_get_the(double, KEY, MAP)

#define fa_map_get_or(TYPE, KEY, DEFAULT, MAP) \
    (fa_map_get(KEY, opts) ? fa_peek_##TYPE(fa_map_get(KEY, MAP)) : DEFAULT)

#define fa_map_get_int32_or(KEY, DEFAULT, MAP) fa_map_get_or(int32, KEY, DEFAULT, MAP)
#define fa_map_get_int64_or(KEY, DEFAULT, MAP) fa_map_get_or(int64, KEY, DEFAULT, MAP)
#define fa_map_get_double_or(KEY, DEFAULT, MAP) fa_map_get_or(double, KEY, DEFAULT, MAP)

#define fa_sizeof_array(A) sizeof(A) / sizeof(A[0])
#define fa_option_show_all(A,S) fa_option_show(fa_sizeof_array(A),A,S)
#define fa_option_parse_all(A,AC,AV) fa_option_parse(fa_sizeof_array(A), A, AC, AV)

int fa_initialize2()
{
    fa_initialize();
    return 0;
}
void fa_terminate2(int x)
{
    fa_terminate();
}


#define fa_with_faudio() \
    fa_with(__faudio__, fa_initialize2(), fa_terminate2(__faudio__))

#define fa_with_options(OPTIONS, ARGC, ARGV, OPTS, ARGS) \
    fa_let(__res__, fa_option_parse_all(OPTIONS, ARGC, (char **) ARGV))   \
        fa_let(OPTS, fa_pair_first(__res__))                              \
            fa_let(ARGS, fa_pair_second(__res__))                         \
                if (                                                      \
                    (OPTS != (ARGS + 1)) && /* Use variables */           \
                    fa_map_has_key(string("h"), OPTS)                     \
                    )                                                     \
                    fa_option_show_all(OPTIONS, (char *) ARGV[0]);        \
                    else                                                  \


fa_option_t option_declaration[] = {
    // { "h", "help",        "Show options",                 NULL },
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
    signal_t a = constant(0);

    int    number_of_nodes = fa_map_get_int32(string("number-of-nodes"), opts);
    int    duration        = fa_map_get_int32(string("duration"),        opts);
    double amplitude       = fa_map_get_double(string("amplitude"),      opts);
    int    frequency       = fa_map_get_int32(string("frequency"),       opts);

    int    sample_rate     = fa_map_get_int32(string("sample-rate"),     opts);
    int    vector_size     = fa_map_get_int32(string("vector-size"),     opts);
    double latency         = fa_map_get_double(string("latency"),        opts);

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
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        list_t out          = list(a, a);

        fa_audio_set_parameter(string("sample-rate"), f64(sample_rate), s);
        fa_audio_set_parameter(string("vector-size"), i32(vector_size), s);
        fa_audio_set_parameter(string("latency"), f64(latency), s);

        mark_used(i);
        mark_used(o);
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

int main(int ac, char const *av[])
{
#ifdef FAUDIO_DEBUG
    fa_set_log_std();
#endif
    fa_with_faudio() {
        fa_with_options(option_declaration, ac, av, options, args) {
            run_sines(options);
        }        
    }
}
