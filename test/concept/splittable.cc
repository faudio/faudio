
#include <utility>
#include <vector>
#include <gtest/gtest.h>
#include <scl/concept/splittable.hpp>

BOOST_CONCEPT_ASSERT((scl::Splittable<std::vector<int>>));

TEST(Concept, Splittable) {}
