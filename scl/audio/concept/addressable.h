
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{       
/*
concept Addressable<typename X>
{
  typename address_type;
  list<Dispatcher<T>> X::get_dispatcher(address_type);
  // Models: portaudio_stream, sndfile_stream
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
