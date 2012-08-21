
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
      ### Synopsis ###

          concept Receiver<typename X>
          {
            typename address_type;
            typename message_type;
            requires (EqualityComparable<address_type>);
            requires (Copyable<message_type>);
            template void X::receive(address_type, MessageTypeRange);
          }

      ### Models ###

      (none)
  */

  template <class X>
  struct Receiver
  {
    BOOST_CONCEPT_USAGE(Receiver)
    {
    }
  private:
    X t;
  };
}
