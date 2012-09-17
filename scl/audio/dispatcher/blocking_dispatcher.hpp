
#pragma once

#include <list>

namespace scl
{
  namespace audio
  {
    namespace dispatcher
    { 

      /** @cond internal */
      
      template <class A, class Address = intptr_t>
      class blocking_dispatcher_base
      {                        
        using receiver_type = any_receiver<message_type, address_type>;
        receiver_type*     sync_recv;
        condition_variable sync_cond;
        mutex              sync_mutex;
      public:
        using address_type = Address;
        using message_type = A;
        
        void receive(address_type address, std::list<message_type> messages)
        {
          unique_lock<mutex> lock(sync_mutex); 
          sync_cond.wait();
          sync_recv.receive(address, messages);
        }
        void send(receiver_type recv)
        {                             
          unique_lock<mutex> lock(sync_mutex);
          sync_recv = recv;
          sync_cond.notify();
        }
      };

      /** @endcond */

      template <class A, class Address = intptr_t>
      class blocking_dispatcher
      {                        
      public:
        typedef Address address_type;
        typedef A       message_type;
        
        template <class MessageTypeRange>
          void receive(address_type address, MessageTypeRange messages);
        template <class MessageTypeReceiver>
          void send(MessageTypeReceiver recv);
      };
    }
  }
}

