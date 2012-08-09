
#include <gtest/gtest.h>
#include <scl/concept/copyable.hpp>

struct copy
{
  copy(const copy&) = default;
  copy& operator =(const copy&) = default;
};
struct no_copy
{
  no_copy(const no_copy&) = delete;
  no_copy& operator =(const no_copy&) = delete;
};

BOOST_CONCEPT_ASSERT((scl::Copyable<int>));
BOOST_CONCEPT_ASSERT((scl::Copyable<copy>));
// BOOST_CONCEPT_ASSERT((scl::Copyable<no_copy>)); // should not compile

TEST(Concept, Copyable) {}
