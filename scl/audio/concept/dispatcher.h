
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{       
/*
concept Dispatcher<typename X> : Receiver<X>, Sender<X>
{
  // Models: blocking_dispatcher, nonblocking_dispatcher, bidirectional_dispatcher
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
