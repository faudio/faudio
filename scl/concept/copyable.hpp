
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  template <class T>
  struct CopyConstructible
  {
    BOOST_CONCEPT_USAGE(CopyConstructible)
    {
      T a(b);
      T* ptr = &a;
      const_constraints(a);
    }
  private:
    void const_constraints(const T& a)
    {
      T c(a);
      const T* ptr = &a;
    }
    T b;
  };

  template <class T>
  struct CopyAssignable
  {
    BOOST_CONCEPT_USAGE(CopyAssignable)
    {
      a = b;
      const_constraints(b);
    }
  private:
    void const_constraints(const T& x)
    {
      a = x;
    }
    T a;
    T b;
  };

  template <class T>
  struct Copyable
      : CopyConstructible<T>
      , CopyAssignable<T>
  {};
}

