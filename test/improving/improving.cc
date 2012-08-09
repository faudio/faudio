
#include <gtest/gtest.h>
#include <scl/improving.hpp>

BOOST_CONCEPT_ASSERT((scl::LessThanComparable<scl::improving<int>>));
  // EqualityComparable<improving<T>>
  // Monoid<improving<T>>

TEST(Improving, Basic)
{
}

