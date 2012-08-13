
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{
  namespace future
  {
    template <class T>
    class Interrupible
    {                 
      BOOST_CONCEPT_USAGE(Interruptible)
      {
      // void interrupt();
      // bool is_interrupted();
      }
    private:
      void const_constrains(const T& a)
      {
        
      } 
      T a;
    };
  }
}

