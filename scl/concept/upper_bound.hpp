
#pragma once

#include <limits>
#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
    Concept describing types with a upper bound.

    Synopsis:

        concept Monoid<typename X>
          : Semigroup<X>
          : DefaultConstructible<X>
        {
        }

    Models:

      - signed char
      - signed int

  */
  template <class T>
  struct UpperBound
  {
    BOOST_CONCEPT_USAGE(UpperBound)
    {
      x = std::numeric_limits<T>::max();
    }
  private:
    T x;
  };
}

