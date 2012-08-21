
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
      ### Synopsis ###

          concept Pausable<typename X> : Abortable<X>
          {
            void X::paus();
            void X::resume();
            bool X::is_running();
          }

      ### Models ###

      future
  */

  template <class X>
  struct Pausable
  {
    BOOST_CONCEPT_USAGE(Pausable)
    {
    }
  private:
    X t;
  };
}
