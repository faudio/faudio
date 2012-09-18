
#pragma once

#include <limits>
#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
    Concept describing types with a lower bound.

    Note that any DefaultConstructible type models LowerBound due to the specification
    of std::numeric_limits in the standard library.

    Synopsis:

        concept Monoid<typename X>
          : Semigroup<X>
          : DefaultConstructible<X>
        {
        }

    Models:
      signed int
  */
  template <class T>
  struct LowerBound
  {
    BOOST_CONCEPT_USAGE(LowerBound)
    {
      x = std::numeric_limits<T>::lowest();
    }
  private:
    T x;
  };
}

