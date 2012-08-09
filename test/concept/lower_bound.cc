
#include <gtest/gtest.h>
#include <scl/concept/lower_bound.hpp>

struct bounded {};
struct not_bounded
{
  not_bounded() = delete;
};

BOOST_CONCEPT_ASSERT((LowerBound<bounded>));
// BOOST_CONCEPT_ASSERT((LowerBound<not_bounded>)); // should not compile

BOOST_CONCEPT_ASSERT((LowerBound<char>));
BOOST_CONCEPT_ASSERT((LowerBound<int>));
BOOST_CONCEPT_ASSERT((LowerBound<float>));
BOOST_CONCEPT_ASSERT((LowerBound<double>));
BOOST_CONCEPT_ASSERT((LowerBound<std::u16string>));
BOOST_CONCEPT_ASSERT((LowerBound<std::u32string>));

TEST(Concept, LowerBound) {}
