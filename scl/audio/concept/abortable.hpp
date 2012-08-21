
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
      ### Synopsis ###

          concept Abortable<typename X>
          {
            void X::abort();
            bool X::is_aborted();
          }

      ### Models ###

      future
  */

  template <class X>
  struct Abortable
  {
    BOOST_CONCEPT_USAGE(Abortable)
    {
    }
  private:
    X t;
  };
}
