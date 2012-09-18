
#pragma once

#include <list>
#include <scl/concept.hpp>

namespace scl
{
  namespace audio
  {
    /**
      Synopsis:

          concept Receiver<typename X>
          {
            typename address_type;
            typename message_type;

            requires (EqualityComparable<address_type>);
            requires (Copyable<message_type>);

            template void X::receive(address_type, MessageTypeRange);
          }

      Models:
        none
    */
    template <class X>
    struct Receiver
    { 
      scl_typename(address_type);
      scl_typename(message_type);

      scl_requires(EqualityComparable<address_type>);
      scl_requires(Copyable<message_type>);
      
      scl_usage(Receiver)
      {                  
        std::list<message_type> msgs;
        for (message_type m : msgs)
          ;
        x.receive(0, msgs);
      }
    private:
      X x;
    };
  }
}

