
#include <gtest/gtest.h>
#include <scl/concept/moveable.hpp>

struct move
{
  move(move &&) = default;
  move& operator =(move &&) = default;
};
struct no_move
{
  no_move(no_move &&) = delete;
  no_move& operator =(no_move &&) = delete;
};

BOOST_CONCEPT_ASSERT((scl::Moveable<int>));
BOOST_CONCEPT_ASSERT((scl::Moveable<move>));
// BOOST_CONCEPT_ASSERT((scl::Moveable<no_move>)); // should not compile

TEST(Concept, Moveable) {}
