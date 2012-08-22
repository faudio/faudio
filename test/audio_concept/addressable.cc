
#include <gtest/gtest.h>
#include <scl/audio/concept/addressable.hpp>

struct not_eq_comp
{
};

struct addressable
{       
  // using address_type = not_eq_comp;
  using address_type = intptr_t;
};
struct not_addressable
{
};

BOOST_CONCEPT_ASSERT((scl::Addressable<addressable>));
// BOOST_CONCEPT_ASSERT((scl::Addressable<not_addressable>)); // should not compile

TEST(AudioConcept, Addressable) {}
