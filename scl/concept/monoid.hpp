
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>
#include <scl/concept/semigroup.hpp>

namespace scl
{
  template <class T>
  struct Monoid
    : Semigroup<T>
    , boost::DefaultConstructible<T>
  {};
}
