
#include <gtest/gtest.h>
#include <scl/improving.hpp>

using namespace scl;

BOOST_CONCEPT_ASSERT((Moveable<improving<int>>));
  // EqualityComparable<improving<T>>
  // Monoid<improving<T>>

TEST(Improving, Basic)
{
}

