
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /*
  concept Sender<typename X>
  {
    typename message_type;
    requires (Copyable<message_type>);
    template void X::send(MessageTypeReceiver);
  }

  */
  template <class X>
  struct Abortable
  {
    BOOST_CONCEPT_USAGE(Timer)
    {
    }
  private:
    X t;
  };
}
