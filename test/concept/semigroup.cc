
#include <string>
#include <gtest/gtest.h>
#include <scl/concept/semigroup.hpp>

struct a_semigroup
{
  a_semigroup() = default;
};
a_semigroup operator+ (const a_semigroup& x, const a_semigroup& y)
{
  return x;
}

struct not_a_semigroup
{
  not_a_semigroup() = delete;
};

BOOST_CONCEPT_ASSERT((scl::Semigroup<a_semigroup>));
// BOOST_CONCEPT_ASSERT((scl::Semigroup<not_a_semigroup>)); // should not compile

BOOST_CONCEPT_ASSERT((scl::Semigroup<char>));
BOOST_CONCEPT_ASSERT((scl::Semigroup<int>));
BOOST_CONCEPT_ASSERT((scl::Semigroup<float>));
BOOST_CONCEPT_ASSERT((scl::Semigroup<double>));
BOOST_CONCEPT_ASSERT((scl::Semigroup<std::string>));
BOOST_CONCEPT_ASSERT((scl::Semigroup<std::u16string>));
BOOST_CONCEPT_ASSERT((scl::Semigroup<std::u32string>));

TEST(Concept, Semigroup) {}
