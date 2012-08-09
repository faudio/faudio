
#include <string>
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

BOOST_CONCEPT_ASSERT((Semigroup<a_semigroup>));
// BOOST_CONCEPT_ASSERT((Semigroup<not_a_semigroup>)); // should not compile

BOOST_CONCEPT_ASSERT((Semigroup<char>));
BOOST_CONCEPT_ASSERT((Semigroup<int>));
BOOST_CONCEPT_ASSERT((Semigroup<float>));
BOOST_CONCEPT_ASSERT((Semigroup<double>));
BOOST_CONCEPT_ASSERT((Semigroup<std::string>));
BOOST_CONCEPT_ASSERT((Semigroup<std::u16string>));
BOOST_CONCEPT_ASSERT((Semigroup<std::u32string>));
