
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

template <class T>
struct Semigroup
{
  BOOST_CONCEPT_USAGE(Semigroup)
  {
    c = a + b;
  }
private:
  T a;
  T b;
  T c;
};

