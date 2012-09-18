
#include <gtest/gtest.h>
#include <scl/concept/lower_bound.hpp>

struct bounded {};
struct not_bounded
{
  not_bounded() = delete;
};

// template <> class std::numeric_limits<bounded>
// {
// public:
// static constexpr bounded min() { return bounded() };
// static constexpr bounded max() { return bounded() };
// static constexpr bounded lowest() { return bounded() };
// };

// BOOST_CONCEPT_ASSERT((scl::LowerBound<bounded>));
// BOOST_CONCEPT_ASSERT((scl::LowerBound<not_bounded>)); // should not compile

BOOST_CONCEPT_ASSERT((scl::LowerBound<char>));
BOOST_CONCEPT_ASSERT((scl::LowerBound<int>));
BOOST_CONCEPT_ASSERT((scl::LowerBound<float>));
BOOST_CONCEPT_ASSERT((scl::LowerBound<double>));
BOOST_CONCEPT_ASSERT((scl::LowerBound<std::u16string>));
BOOST_CONCEPT_ASSERT((scl::LowerBound<std::u32string>));

TEST(Concept, LowerBound) {}
