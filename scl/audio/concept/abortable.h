
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /*
  concept Abortable<typename X>
  {
    void X::abort();
    bool X::is_aborted();
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
