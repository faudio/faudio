
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /*
  concept Stream<typename X>
    : Startable<X>
    , Semigroup<X>
    , Addressable<X>
  {
    typename session_type;
    typename device_type;
    typename time_type;
    typename processor_type;
    typename result_type = processor_type::result_type;
                          X::X(device_type& in, processor_type& proc, device_type& out);
                          ~X::X();
    improving<time_type>  X::time();
    time_type             X::length();
    improving<time_type>  X::progress(); // progress() == time() / length()
    future<result_type>   X::result();
  }

  */

  template <class X>
  struct Stream
  {
    BOOST_CONCEPT_USAGE(Stream)
    {
    }
  private:
    X t;
  };
}
