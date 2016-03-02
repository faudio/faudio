#define FAUDIO_NO_INFO_LOG

#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>
#include "common.h"

/*
    This program tries to open a stream to each available audio device,
    one at a time.

 */

fa_option_t option_declaration[] = {
    { "d", "duration",        "Duration",           fa_option_integral, "2000"  },
    { "f", "frequency",       "Frequency",          fa_option_integral, "415"   },
    { "a", "amplitude",       "Amplitude",          fa_option_floating, "0.1"   },
    { "n", "number-of-nodes", "Number of nodes",    fa_option_integral, "1"     },

    { "r", "sample-rate",     "Sample rate",        fa_option_integral, "44100" },
    { "v", "vector-size",     "Vector size",        fa_option_integral, "64"    },
    { "l", "latency",         "Latency",            fa_option_floating, "0.040" },
    { "V", "verbosity",       "How much to print",  fa_option_string,   "warning" }
};

void print_device(fa_audio_device_t x)
{
    printf("===== %s  %s =================", fa_unstring(fa_audio_host_name(x)), fa_unstring(fa_audio_name(x)));
    printf("  In: %d  Out: %d   Default Sample Rate: %d\n",
        fa_audio_input_channels(x), fa_audio_output_channels(x),
        (int) fa_audio_default_sample_rate(x));
}

fa_signal_t make_sines(int node_count, double frequency, double amplitude)
{
    fa_signal_t sines = fa_constant(0);

    {
        double f = frequency;
        double x = amplitude;

        for (int i = 0; i < node_count; ++i) {

            /*
              Build a left-leaning tree, so that the depth is equal to the number of nodes as
              passed on the command line. We could get away with a much larger number
              of nodes by using a balanced tree.
            */

            sines = fa_add(fa_multiply(fa_signal_sin(fa_signal_line(f)), fa_constant(x)), sines);
            f *= 1.12;
            x *= 0.84;
        }
    }
    return sines;
}

fa_audio_session_t test_audio_devices(fa_ptr_t options, fa_audio_session_t session)
{
    fa_map_t opts = (fa_map_t)options;
    
    const int  duration        = fa_map_get_int32(fa_string("duration"),          opts);
    const int  frequency       = fa_map_get_int32(fa_string("frequency"),         opts);
    const double amplitude     = fa_map_get_double(fa_string("amplitude"),        opts);
    const int  number_of_nodes = fa_map_get_int32(fa_string("number-of-nodes"),   opts);
    const int  sample_rate     = fa_map_get_int32(fa_string("sample-rate"),       opts);
    const int  vector_size     = fa_map_get_int32(fa_string("vector-size"),       opts);
    const double latency       = fa_map_get_double(fa_string("latency"),          opts);
    
    
    fa_audio_set_parameter(fa_string("sample-rate"), fa_f64(sample_rate), session);
    fa_audio_set_parameter(fa_string("vector-size"), fa_i32(vector_size), session);
    fa_audio_set_parameter(fa_string("latency"),     fa_f64(latency),     session);
    
    printf("TESTING OUTPUT\n\n");
    fa_for_each(x, fa_audio_all(session)) {
        if (!fa_check(x)) {
            print_device(x);
            
            if (fa_audio_output_channels(x)) {
                printf("Opening stream in %d Hz...\n", (int)sample_rate);
                
                fa_signal_t sines = make_sines(number_of_nodes, frequency, amplitude);
                
                fa_open_stereo_out(stream, x, list(sines, sines)) {
                    fa_thread_sleep(duration);
                }
                
                
            } else {
                printf("No outputs, skipping\n");
            }
            printf("\n");
        }
    }
    
    printf("\nDONE\n");

    return session;
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_error_severity_t level = warning;
    fa_with_options(option_declaration, argc, argv, options, args) {
        fa_string_t level_str = fa_map_dget(fa_string("verbosity"), options);
        if (fa_equal(level_str, fa_string("error"))) {
            level = error;
        } else if (fa_equal(level_str, fa_string("warning"))) {
            level = warning;
        } else if (fa_equal(level_str, fa_string("info"))) {
            level = info;
        } else {
            fa_slog_error("Unknown verbosity level ", level_str);
            fa_slog_info("(Valid values are info, warning and error)");
        }
        fa_set_log_level(level);
        
        fa_with_faudio() {
            fa_audio_with_session(
                test_audio_devices, options,
                fa_error_log, NULL);
        }
    }
}

