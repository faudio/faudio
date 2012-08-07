//
// #pragma once
//
// #include <boost/concept_check.hpp>
// #include <boost/concept/assert.hpp>
// #include <boost/concept/requires.hpp>
//
// template <class T>
// struct MoveConstructible
// {
//   BOOST_CONCEPT_USAGE(MoveConstructible) {
//     T a(std::move(b)); // require move constructor
//     // T* ptr = &a;       // require address of operator
//     const_constraints(a);
//     // ignore_unused_variable_warning(ptr);
//   }
//  private:
//   void const_constraints(const T& a) {
//     T c(std::move(a));            // require const move constructor
//     // const T* ptr = &a; // require const address of operator
//     // ignore_unused_variable_warning(c);
//     // ignore_unused_variable_warning(ptr);
//   }
//   T b;
// };
//
// template <class T>
// struct MoveAssignable
// {
//   BOOST_CONCEPT_USAGE(MoveAssignable) {
//     a = std::move(b);             // require assignment operator
//     const_constraints(b);
//   }
//  private:
//   void const_constraints(const T& x) {
//     a = std::move(x);              // const required for argument to assignment
//   }
//  private:
//   T a;
//   T b;
// };
//
// template <class T>
// struct Moveable
//   : MoveConstructible<T>
//   , MoveAssignable<T>
// {};
//
