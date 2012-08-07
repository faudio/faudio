
#include <gtest/gtest.h>
#include <scl/concept/copyable.hpp>

struct copy_me
{
  copy_me(const copy_me&) = default;
  copy_me& operator =(const copy_me&) = default;
};
struct dont_copy_me
{
  dont_copy_me(const dont_copy_me&) = delete;
  dont_copy_me& operator =(const dont_copy_me&) = delete;
};

BOOST_CONCEPT_ASSERT((Copyable<int>));
BOOST_CONCEPT_ASSERT((Copyable<copy_me>));
// BOOST_CONCEPT_ASSERT((Copyable<dont_copy_me>)); // should not compile

