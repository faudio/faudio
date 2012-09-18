
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>
#include <scl/concept/semigroup.hpp>

namespace scl
{
  using boost::DefaultConstructible;

  /**
    Synopsis:

        concept Monoid<typename X>
          : Semigroup<X>
          : DefaultConstructible<X>
        {
        }

    Models:
      int,
      double,
      std::list
  */
  template <class T>
  struct Monoid
      : Semigroup<T>
      , DefaultConstructible<T> {};
}
