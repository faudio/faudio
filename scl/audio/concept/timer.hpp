
#pragma once

#include <scl/concept.hpp>

namespace scl
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
  template <class X>
  struct Timer
  {
    using time_type = X::time_type;
    BOOST_CONCEPT_ASSERT((LessThanComparable<address_type>));
    BOOST_CONCEPT_ASSERT((LowerBounded<address_type>));
    BOOST_CONCEPT_USAGE(Timer)
    {
      time_type = x.time();
    }
  private:
    X x;
  };
}
