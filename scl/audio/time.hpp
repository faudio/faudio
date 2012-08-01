
#include <boost/rational.hpp>

namespace scl
{
  namespace audio
  {
    enum time_unit
    {
      hours;
      minutes;
      seconds;
      milliseconds;
      microseconds;
    };

    //! Used for non-sample-accurate timing
    struct approxime
    {
      double    time;
      time_unit unit;
    };

    // Used for sample-accurate timing
    struct time
    {
      rational<int64> time;
      time_unit       unit;
    };

    // Used for sample-accurate timing, faster than time where sample rate is known
    struct sampleime
    {
      int64 sample_count;
      time  sample_rate;
    };
  }
}

