// 
// #include <gtest/gtest.h>
// #include <scl/concept/moveable.hpp>
// 
// struct move_me
// {
//   move_me(const move_me&&) = default;
//   move_me& operator =(const move_me&&) = default;
// };
// struct dont_move_me
// {
//   dont_move_me(const dont_move_me&&) = delete;
//   dont_move_me& operator =(const dont_move_me&&) = delete;
// };
// 
// BOOST_CONCEPT_ASSERT((scl::Copyable<int>));
// BOOST_CONCEPT_ASSERT((scl::Copyable<move_me>));
// // BOOST_CONCEPT_ASSERT((scl::Copyable<dont_move_me>)); // should not compile
// 
// 
// TEST(Concept, Moveable) {}
