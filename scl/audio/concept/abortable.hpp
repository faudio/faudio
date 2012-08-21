
#pragma once

#include <scl/concept.hpp>

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
      x.abort();
      bool state = x.is_aborted();
    }
  private:
    X x;
  };
}
