
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Pausable<typename X> : Abortable<X>
        {
          void X::paus();
          void X::resume();
          bool X::is_running();
        }

    Models:
      future
  */
  template <class X>
  struct Pausable : Abortable<X>
  {
    BOOST_CONCEPT_USAGE(Pausable)
    {
    }
  private:
    X t;
  };
}
