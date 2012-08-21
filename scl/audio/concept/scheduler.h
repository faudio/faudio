
#pragma once

#include <boost/concept_check.hpp>
#include <boost/concept/assert.hpp>
#include <boost/concept/requires.hpp>

namespace scl
{       
/*
concept Scheduler<typename X>
{
  typename task_type
  typename time_type = timer_type::time_type;
  requires (Callable<task_type>);
  requires (Moveable<task_type>);
  requires (LessThanComparable<time_type>);
  requires (LowerBounded<time_type>);

                            X::X(const improving<time_type>&);
  template <TimeTypeRange>
    future<value_type>      X::schedule(task_type&&, TimeTypeRange);
  void                      X::perform();
  // Models: locked_scheduler, unlocked_scheduler
}

*/
  
  template <class X>
  struct Abortable
  {
    BOOST_CONCEPT_USAGE(Timer)
    {  
    }
  private:
    X t;
  };
}
