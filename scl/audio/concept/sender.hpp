
#pragma once

#include <scl/concept.hpp>
#include <scl/audio/concept/receiver.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Sender<typename X>
        {
          typename message_type;

          requires (Copyable<message_type>);

          template void X::send(MessageTypeReceiver);
        }

    Models: none
  */
  template <class X>
  struct Sender
  {
    using message_type = X::message_type;
    BOOST_CONCEPT_ASSERT((Copyable<message_type>));
    BOOST_CONCEPT_USAGE(Sender)
    {
    }
  private:
    X t;
  };
}
