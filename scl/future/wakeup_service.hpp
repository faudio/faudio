
#pragma once

#include <scl/atomic.hpp>
#include <scl/thread.hpp>
#include <scl/concept.hpp>

namespace scl
{
  namespace future
  {
    namespace wakeup_service
    {
      using ::scl::atomic::atomic_int;
      using ::scl::thread::mutex;
      using ::scl::thread::condition_variable;

      template <class Event>
      struct wakeup_service_state
      {
        using event_type = Event;

        wakeup_service_state()
          : blocked(0)
          , pending(0) {}

        event_type         event;
        mutex              mutex;
        condition_variable condition;
        atomic_int         blocked; // number of threads blocking
        atomic_int         pending; // number of threads waiting to read update condition
      };

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
        using state_type     = wakeup_service_state<event_type>;
      public:

        wakeup_service() : state(new state_type) {}
        wakeup_service(const this_type& other) = delete;
        wakeup_service(this_type && other) = delete;
        ~wakeup_service() = default;

        this_type& operator= (const this_type& other) = delete;
        this_type& operator= (this_type && other) = delete;

        class error : public std::exception
        {
          const char* what() const noexcept
          {
            return "Inconsistent state in wakeup_service";
          }
        };

        /** Block until an event mathcing the predicate occurs */
        void sleep(std::function<bool(event_type)> predicate);

        /** Signal the given event */
        void wake(event_type event);

      private:
        std::unique_ptr<state_type> state;
      };

    }
  }

  using future::wakeup_service::wakeup_service;
  
  template <class T>
  void wakeup_service<T>::sleep(std::function<bool(event_type)> predicate)
  {
    using thread::mutex;
    using thread::unique_lock;
    if (!state) throw error();
    {
      unique_lock<mutex> lock(state->mutex);
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

  template <class T>
  void wakeup_service<T>::wake(event_type event)
  {
    using thread::mutex;
    using thread::unique_lock;
    if (!state) throw error();
    while (state->pending > 0); // busy-wait until former wakes have been propagated
    {
      unique_lock<mutex> lock(state->mutex);
      std::cout << "wake state is : " << state.get() << "\n";
      state->event  = event;
      int b = state->blocked;
      state->pending = b;
    }
    state->condition.notify_all();
  }
}

