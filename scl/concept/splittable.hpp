
#pragma once

#include <utility>
#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>
#include <scl/parallel/split.hpp>

template <class T>
struct Splittable
{
  BOOST_CONCEPT_USAGE(Splittable)
  {
    p = scl::split(a);
  }
private:
  std::pair<T,T> p;
  T a;
};

