
#pragma once

#include <scl/concept.hpp>
#include <scl/audio/concept/receiver.hpp>

namespace scl
{
  namespace audio
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
      scl_typename(message_type);
      scl_requires(Copyable<message_type>);
      
      scl_usage(Sender)
      {             
        // TODO
        // class test_receiver {};       
        // x.send();
      }
    private:
      X t;
    };
  }
}

