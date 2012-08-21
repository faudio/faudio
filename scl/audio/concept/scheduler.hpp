
#pragma once

#include <scl/concept.hpp>

namespace scl
{
  /**
      ### Synopsis ###

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
          }

      ### Models ###

      locked_scheduler, unlocked_scheduler
  */

  template <class X>
  struct Scheduler
  {
    BOOST_CONCEPT_USAGE(Scheduler)
    {
    }
  private:
    X t;
  };
}
