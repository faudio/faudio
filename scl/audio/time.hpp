
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

    constexpr unsigned long conversion_factor(time_unit t)
    {
      switch (t)
      {
      case day:
        return 1000 * 1000 * 60 * 60 * 24;
      case hours:
        return 1000 * 1000 * 60 * 60;
      case minutes:
        return 1000 * 1000 * 60;
      case seconds:
        return 1000 * 1000;
      case milliseconds:
        return 1000;
      case microseconds:
        return 1;
      default:
        assert(false);
      }
    }

    //! Used for non-sample-accurate timing
    struct approximate_time
    {
      double    time;
      time_unit unit;
    };

    // Used for sample-accurate timing
    struct time
    {
      rational<int64_t> time;
      time_unit         unit;
    };

    // Used for sample-accurate timing, faster than time where sample rate is known
    struct sample_time
    {
      int64_t sample_count;
      time    sample_rate;
    };
  }
}

