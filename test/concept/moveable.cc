//
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
// BOOST_CONCEPT_ASSERT((Copyable<int>));
// BOOST_CONCEPT_ASSERT((Copyable<move_me>));
// // BOOST_CONCEPT_ASSERT((Copyable<dont_move_me>)); // should not compile
//
