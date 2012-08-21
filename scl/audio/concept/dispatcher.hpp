
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
      ### Synopsis ###

          concept Dispatcher<typename X> : Receiver<X>, Sender<X>
          {
          }

      ### Models ###

      blocking_dispatcher, nonblocking_dispatcher, bidirectional_dispatcher
  */

  template <class X>
  struct Dispatcher
  {
    BOOST_CONCEPT_USAGE(Dispatcher)
    {
    }
  private:
    X t;
  };
}
