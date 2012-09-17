
#pragma once

namespace scl
{
  namespace audio
  {
    namespace dispatcher
    {                
      
      /** @cond internal */
      
      template <class A, class Address = intptr_t>
      class left_dispatcher
      {                        
        using dispatcher_type = any_dispatcher<message_type, address_type>;
        dispatcher_type x;
        dispatcher_type y;
      public:
        using address_type = Address;
        using message_type = A;
        
        left_dispatcher(dispatcher_type x, dispatcher_type y)
          : x(x), y(y) {}
        
        void receive(address_type address, std::list<message_type> messages)
        {
          x.receive(address, messages);
        }
        void send(receiver_type recv)
        {                              
          y.send(recv);
        }
      };
      
      template <class A, class Address = intptr_t>
      class left_dispatcher
      {                        
        using dispatcher_type = any_dispatcher<message_type, address_type>;
        dispatcher_type x;
        dispatcher_type y;
      public:
        using address_type = Address;
        using message_type = A;
        
        left_dispatcher(dispatcher_type x, dispatcher_type y)
          : x(x), y(y) {}
        
        void receive(address_type address, std::list<message_type> messages)
        {
          y.receive(address, messages);
        }
        void send(receiver_type recv)
        {                              
          x.send(recv);
        }
      };

      /** @endcond */
      
      class bidirectional_dispatcher {};
    }
  }
}

