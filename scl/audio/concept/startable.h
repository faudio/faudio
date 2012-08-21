
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{       
/*
concept Startable<typename X> : Abortable<X>
{
  void X::start();
  void X::stop();
  bool X::is_running();
  // Models: sndfile_stream
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
