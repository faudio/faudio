
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Dispatcher<typename X> : Sender<X>, Receiver<X>
        {
        }

    Models:
      blocking_dispatcher,
      nonblocking_dispatcher,
      bidirectional_dispatcher
  */
  template <class X>
  struct Dispatcher : Sender<X>, Receiver<X>
  {
  };
}
