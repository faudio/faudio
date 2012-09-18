
#pragma once

#include <scl/concept/copyable.hpp>
#include <scl/concept/moveable.hpp>
#include <scl/concept/semigroup.hpp>
#include <scl/concept/monoid.hpp>
#include <scl/concept/lower_bound.hpp>
#include <scl/concept/splittable.hpp>

#define scl_concept(X)  template <class X>
#define scl_typename(T) using T = typename X::T
#define scl_requires(E) BOOST_CONCEPT_ASSERT((E))
#define scl_usage(X)    public: BOOST_CONCEPT_USAGE(X)

namespace scl
{
  using boost::EqualityComparable;
  using boost::LessThanComparable;
  using boost::DefaultConstructible;

  // TODO add all...
}

