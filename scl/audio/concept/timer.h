
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{       
/*
concept Timer<typename X>
{
  typename time_type;
  requires (LessThanComparable<time_type>);
  requires (LowerBounded<time_type>);
  improving<time_type> X::time();
}

*/
  
  template <class X>
  struct Timer
  {
    BOOST_CONCEPT_USAGE(Timer)
    {  
    }
  private:
    X t;
  };
}
