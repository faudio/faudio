
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{       
/*
concept Receiver<typename X>
{
  typename address_type;
  typename message_type;
  requires (EqualityComparable<address_type>);
  requires (Copyable<message_type>);
  template void X::receive(address_type, MessageTypeRange);
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
