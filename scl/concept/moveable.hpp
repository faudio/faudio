
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  template <class T>
  struct MoveConstructible
  {
    BOOST_CONCEPT_USAGE(MoveConstructible)
    {
      T a(std::move(b));
    }
  private:
    void const_constraints(const T& a)
    {
    }
    T b;
  };

  template <class T>
  struct MoveAssignable
  {
    BOOST_CONCEPT_USAGE(MoveAssignable)
    {
      a = std::move(b);
    }
  private:
    void const_constraints(const T& x)
    {
    }
  private:
    T a;
    T b;
  };

  template <class T>
  struct Moveable
    : MoveConstructible<T>
    , MoveAssignable<T> {};
}

