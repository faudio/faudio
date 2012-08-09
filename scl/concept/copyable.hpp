
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
      T a(b);             // require copy constructor
      T* ptr = &a;        // require address of operator
      const_constraints(a);
    }
  private:
    void const_constraints(const T& a)
    {
      T c(a);             // require const copy constructor
      const T* ptr = &a;  // require const address of operator
    }
    T b;
  };

  template <class T>
  struct CopyAssignable
  {
    BOOST_CONCEPT_USAGE(CopyAssignable)
    {
      a = b;              // require assignment operator
      const_constraints(b);
    }
  private:
    void const_constraints(const T& x)
    {
      a = x;              // const required for argument to assignment
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

