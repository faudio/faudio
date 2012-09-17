
#pragma once

#include <scl/concept.hpp>
#include <scl/audio/concept/sender.hpp>
#include <scl/audio/concept/receiver.hpp>

namespace scl
{
  namespace audio
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
}

