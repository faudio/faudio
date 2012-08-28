
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  namespace audio
  {
  /**
    Synopsis:

        concept Timer<typename X>
        {
          typename time_type;

          requires (LessThanComparable<time_type>);
          requires (LowerBounded<time_type>);

          improving<time_type> X::time();
        }

    Models:
      none
  */
  scl_concept(X) struct Timer
  {
    scl_typename(time_type);

    scl_requires(LessThanComparable<address_type>);
    scl_requires(LowerBounded<address_type>);
    scl_requires(Timer)

    X x;
    scl_usage(Timer)
    {
      improving<time_type> t = x.time();
    }
  };
}
}

