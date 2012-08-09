
#pragma once

#include <limits>
#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

/**
    A type with a lower bound. Note that any DefaultConstructible type models LowerBound.
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

