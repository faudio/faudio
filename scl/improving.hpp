
#pragma once

#include <map>
#include <iostream>
#include <scl/atomic.hpp>
#include <scl/thread.hpp>
#include <scl/concept.hpp>

namespace scl
{
  /**
      An *improving value* is the continuous version of a future. While a future only have two
      states: not available or available, an improving value generalizes this by proving a
      success of monotonically increasing states.

      Eventually, an improving value may become *fixed*, meaning that it will not improve
      further. A thread holding an improving value may poll the current state, or block until
      the value becomes fixed. As with std::future, wait blocks forever if this does not happen.

  */
//   template <class T>
//   class improving
//   {
//   public:
// #ifdef DOXYGEN_SHOULD_SKIP_THIS
//     BOOST_CONCEPT_ASSERT((LowerBound<T>));
//     BOOST_CONCEPT_ASSERT((LessThanComparable<T>));
// #endif
//     using value_type = T;
//     using this_type  = improving<T>;
//
//     improving() noexcept;        // Fixed at std::numeric_limits::lowest()
//     improving(const T) noexcept; // Fixed at the given value
//     improving(this_type &&);
//     ~improving() noexcept;
//
//     void operator =(improving &&);
//     void swap(this_type& other);
//
//     bool known() const noexcept;
//     T value() const;
//     void wait() const;
//     thread::future<T> to_future() const;
//
//   private:
//     // accumulator
//   };
//
//   template <class T>
//   improving<T>::improving()
//     : current(std::numeric_limits<T>::lowest()) {}
//
//   /*
//     wait()
//     {
//       block on cond var
//        wake up if (current > max)
//     }
//
//     known()
//     {
//       current.get() == max.get();
//     }
//
//     min(x, y)
//     {
//       return improving(
//         max(this->current, other.current),
//         max(this->max, other.max)
//         accumulator
//         );
//     }
//   */

  // /**
  //     The improving analogue to a promise.Can improve or fix the improving value, or set an
  //     exception that will be transported to the reading thread.
  //  */
  // template <class T>
  // class accumulator
  // {
  // public:
  //   improving<T> get_improving() const;
  //
  //   void fix()
  //   {
  //     max.exchange(current.get());
  //     wakeup.notify();
  //   }
  //   void increment(const T& amount)
  //   {
  //     // Can be specialized for AtomicIntegral types
  //     // increment atomically
  //   }
  //   void increment_and_fix(const T& amount)
  //   {
  //     // atomically
  //       // increment
  //       // max := current
  //     wakeup.notify();
  //   }
  //
  // private:
  //   atomic::atomic<T> current;
  //   atomic::atomic<T> max;
  //   wakeup_service() wakeup;
  // };

  // sleep :: (Event -> Bool) -> ()
  // wake  :: Event -> ()

  namespace detail
  {
    template <class Event>
    struct wakeup_service_state
    {      
      wakeup_service_state() 
        : blocked(0)
        , pending(0) 
      {
        std::cout << "Wakeup state created!\n";
      }
      // ~wakeup_service_state()
      // {
      //   std::cout << "Wakeup state deleted!\n";
      // }    
      using event_type = Event;
      event_type event;
      thread::mutex mutex;
      thread::condition_variable condition;
      atomic::atomic_int blocked; // number of threads blocking
      atomic::atomic_int pending; // number of threads waiting to read update condition
    };
  }

  /** Provides composable blocking

      Similar to a condition variable, with the difference that a thread may block on
      a series of wakeups.
      
      Construction, destruction and moving is not thread-safe.
    */
  template <class Event>
  class wakeup_service
  {
  public:
    BOOST_CONCEPT_ASSERT((DefaultConstructible<Event>));
    BOOST_CONCEPT_ASSERT((Copyable<Event>));
    using event_type     = Event;
    using this_type      = wakeup_service<event_type>;
    using predicate_type = std::function<bool(event_type)>;
  private:
    using state_type     = detail::wakeup_service_state<event_type>;
  public:    
    
    wakeup_service()
      : state(new state_type) {}
    // wakeup_service(this_type&& other)
    //   : state(std::move(other.state)) 
    //   {
    //     other.state.reset();
    //   }

    // this_type& operator= (this_type&& other)
    // {
    //   this_type temp (std::move(other));
    //   temp.swap(*this);
    //   return *this;
    // }

    // void swap(this_type& other)
    // {
    //   std::swap(state, other.state);
    // }
    
    class error : public std::exception
    {
      const char* what() const noexcept
      {
        return "Inconsistent state in wakeup_service";
      }
    };

    // Block until an event mathcing the predicate occurs
    void sleep(std::function<bool(event_type)> predicate)
    {
      using thread::mutex;
      using thread::unique_lock;
      if (!state) throw error();
      {
        unique_lock<mutex> lock (state->mutex);
        std::cout << "sleep state is : " << state.get() << "\n";
        state->blocked += 1;
        do
        {
          state->condition.wait(lock);
          state->pending -= 1;
        }
        while (!predicate(state->event));
        state->blocked -= 1;
      }
    }

    // Signal the given event
    void wake(event_type event)
    {
      using thread::mutex;
      using thread::unique_lock;
      if (!state) throw error();
      while (state->pending > 0); // busy-wait until the wake has been propagated
      {                              
        unique_lock<mutex> lock (state->mutex);
        std::cout << "wake state is : " << state.get() << "\n";
        state->event  = event;
        int b = state->blocked;
        state->pending = b;
      }
      state->condition.notify_all();
    }

  private:                                  
    std::unique_ptr<state_type> state;
  };

}
