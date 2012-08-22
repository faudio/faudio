
#pragma once

#include <scl/concept.hpp>

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
    BOOST_CONCEPT_USAGE(Sender)
    {
    }
  private:
    X t;
  };
}
