
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
      ### Synopsis ###

          concept Addressable<typename X>
          {
            typename address_type;
            list<Dispatcher<T>> X::get_dispatcher(address_type);
          }

      ### Models ###

      portaudio_stream, sndfile_stream
  */

  template <class X>
  struct Addressable
  {
    BOOST_CONCEPT_USAGE(Addressable)
    {
    }
  private:
    X t;
  };
}
