
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
      ### Synopsis ###

          concept Startable<typename X> : Abortable<X>
          {
            void X::start();
            void X::stop();
            bool X::is_running();
          }

      ### Models ###

      sndfile_stream
  */

  template <class X>
  struct Startable : Abortable<X>
  {
    BOOST_CONCEPT_USAGE(Startable)
    {
    }
  private:
    X t;
  };
}
