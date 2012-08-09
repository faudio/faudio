
#include <string>
#include <gtest/gtest.h>
#include <scl/concept/monoid.hpp>

struct a_monoid
{
  a_monoid() = default;
};
a_monoid operator+ (const a_monoid& x, const a_monoid& y)
{
  return x;
}

struct not_a_monoid
{
  not_a_monoid() = delete;
};

BOOST_CONCEPT_ASSERT((scl::Monoid<a_monoid>));
// BOOST_CONCEPT_ASSERT((scl::Monoid<not_a_monoid>)); // should not compile

BOOST_CONCEPT_ASSERT((scl::Monoid<char>));
BOOST_CONCEPT_ASSERT((scl::Monoid<int>));
BOOST_CONCEPT_ASSERT((scl::Monoid<float>));
BOOST_CONCEPT_ASSERT((scl::Monoid<double>));
BOOST_CONCEPT_ASSERT((scl::Monoid<std::string>));
BOOST_CONCEPT_ASSERT((scl::Monoid<std::u16string>));
BOOST_CONCEPT_ASSERT((scl::Monoid<std::u32string>));

TEST(Concept, Monoid) {}
