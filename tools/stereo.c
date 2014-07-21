
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>
#include "common.h"

/*
    This program does plays alternating inpulses the left and right speaker.
 */
    fa_option_t option_declaration[] = {
      { "d", "duration",        "Duration",         fa_option_integral, "5000"  },
      { "f", "frequency",       "Frequency",        fa_option_integral, "410"   },
      { "a", "amplitude",       "Amplitude",        fa_option_floating, "0.1"   },

      { "r", "sample-rate",     "Sample rate",      fa_option_integral, "44100" },
      { "v", "vector-size",     "Vector size",      fa_option_integral, "64"    },
      { "l", "latency",         "Latency",          fa_option_floating, "0.040" },
    };

void play_impulses(map_t opts)
{
    const int  duration        = fa_map_get_int32(fa_string("duration"),          opts);
    const int  frequency       = fa_map_get_int32(fa_string("frequency"),         opts);
    // const double amplitude     = fa_map_get_double(fa_string("amplitude"),        opts);

    const int  sample_rate     = fa_map_get_int32(fa_string("sample-rate"),       opts);
    const int  vector_size     = fa_map_get_int32(fa_string("vector-size"),       opts);
    const double latency       = fa_map_get_double(fa_string("latency"),      opts);

    signal_t /*train, */left, right;

    // train  = fa_multiply(fa_constant(1), fa_signal_impulses(44100 / frequency));
    left   = fa_signal_delay(0,                           fa_signal_impulses(44100 / frequency));
    right  = fa_signal_delay(0 + (44100 / (frequency*2)), fa_signal_impulses(44100 / frequency));
    // right = fa_signal_delay(22050, fa_signal_impulses(44100));

    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_set_parameter(fa_string("sample-rate"), f64(sample_rate), s);
        fa_audio_set_parameter(fa_string("vector-size"), i32(vector_size), s);
        fa_audio_set_parameter(fa_string("latency"),   f64(latency),   s);

        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_audio_stream_t st = fa_audio_open_stream(NULL, o, just, list(left, right));

        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        fa_thread_sleep(duration);
        fa_audio_end_session(s);
    }
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {
        fa_with_options(option_declaration, argc, argv, options, args) {
            play_impulses(options);
        }
    }
}
