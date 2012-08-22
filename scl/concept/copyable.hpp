
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  /**
    Synopsis:

        concept CopyConstructible<typename X>
        {
        }

    Models:
      int,
      double,
      std::list
  */
  template <class X>
  struct CopyConstructible
  {
    BOOST_CONCEPT_USAGE(CopyConstructible)
    {
      X a(b);
      X* ptr = &a;
      const_constraints(a);
    }
  private:
    void const_constraints(const X& a)
    {
      X c(a);
      const X* ptr = &a;
    }
    X b;
  };

  /**
    Synopsis:

        concept CopyAssignable<typename X>
        {
        }

    Models:
      int,
      double,
      std::list
  */
  template <class X>
  struct CopyAssignable
  {
    BOOST_CONCEPT_USAGE(CopyAssignable)
    {
      a = b;
      const_constraints(b);
    }
  private:
    void const_constraints(const X& x)
    {
      a = x;
    }
    X a;
    X b;
  };

  /**
    Synopsis:

        concept Copyable<typename X>
          : CopyConstructible<X>
          , CopyAssignable<X>
        {
        }

    Models:
      int,
      double,
      std::list
  */
  template <class X>
  struct Copyable
      : CopyConstructible<X>
      , CopyAssignable<X> {};
}

