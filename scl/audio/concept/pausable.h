
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /*
  concept Pausable<typename X> : Abortable<X>
  {
    void X::paus();
    void X::resume();
    bool X::is_running();
    // Models: future
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
