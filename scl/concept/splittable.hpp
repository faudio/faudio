
#pragma once

#include <utility>
#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>
#include <scl/parallel/split.hpp>

namespace scl
{
  /**
    Synopsis:

        concept Copyable<typename X>
          : CopyConstructible<X>
          , CopyAssignable<X>
        {
        }

    Models:
      int,
      double,
      std::list
  */
  template <class T>
  struct Splittable
  {
    BOOST_CONCEPT_USAGE(Splittable)
    {
      p = scl::split(a);
    }
  private:
    std::pair<T, T> p;
    T a;
  };
}
