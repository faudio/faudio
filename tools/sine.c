
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>
#include "common.h"

/*
  This program plays one of more sines waves on the standard audio output device.

  Options:
    -d        --duration             Duration (default 5000)
    -f        --frequency            Frequency (default 410)
    -a        --amplitude            Amplitude (default 0.1)
    -n        --number-of-nodes      Number of nodes (default 1)
    -r        --sample-rate          Sample rate (default 44100)
    -v        --vector-size          Vector size (default 64)
    -l        --latency              Latency (default 0.040)
 */

fa_option_t option_declaration[] = {
  { "d", "duration",        "Duration",         fa_option_integral, "5000"  },
  { "f", "frequency",       "Frequency",        fa_option_integral, "410"   },
  { "a", "amplitude",       "Amplitude",        fa_option_floating, "0.1"   },
  { "n", "number-of-nodes", "Number of nodes",  fa_option_integral, "1"     },

  { "r", "sample-rate",     "Sample rate",      fa_option_integral, "44100" },
  { "v", "vector-size",     "Vector size",      fa_option_integral, "64"    },
  { "l", "latency",         "Latency",          fa_option_floating, "0.040" },
};

void run_sines(map_t opts)
{
  const int  duration        = fa_map_get_int32(fa_string("duration"),          opts);
  const int  frequency       = fa_map_get_int32(fa_string("frequency"),         opts);
  const double amplitude     = fa_map_get_double(fa_string("amplitude"),        opts);
  const int  number_of_nodes = fa_map_get_int32(fa_string("number-of-nodes"),   opts);
  const int  sample_rate     = fa_map_get_int32(fa_string("sample-rate"),       opts);
  const int  vector_size     = fa_map_get_int32(fa_string("vector-size"),       opts);
  const double latency       = fa_map_get_double(fa_string("latency"),      opts);

  signal_t sines = fa_constant(0);

  {
    double f = frequency;
    double x = amplitude;

    for (int i = 0; i < number_of_nodes; ++i) {

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

  fa_with_session_(session) {
    fa_with_default_out(session, output) {
      fa_audio_set_parameter(fa_string("sample-rate"), f64(sample_rate), session);
      fa_audio_set_parameter(fa_string("vector-size"), i32(vector_size), session);
      fa_audio_set_parameter(fa_string("latency"),   f64(latency),   session);
      fa_open_stereo_out(stream, output, list(sines, sines)) {
        if (duration < 0) {
          while (1) {
            fa_thread_sleep(10000);
          }
        } else {
          fa_thread_sleep(duration);
        }
      }
    }
  }
}

int main(int argc, char const *argv[])
{
  fa_set_log_tool();
  fa_with_faudio() {
    fa_with_options(option_declaration, argc, argv, options, args) {
      fa_inform(fa_string("Running sine test"));
      run_sines(options);
    }
  }
}
