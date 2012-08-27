
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

      /** @cond internal */

      template <class Event>
      struct wakeup_service_state
      {
        using event_type = Event;

        wakeup_service_state()
          : blocked(0)
          , pending(0) {}

        event_type         event;
        mutex              mutx;
        condition_variable condition;
        atomic_int         blocked; // number of threads blocking
        atomic_int         pending; // number of threads waiting to read update condition
      };

      /** @endcond */

      /** Provides composable blocking, allowing a thread to wait for a *set* of events
          instead of a single event as in a condition variable. As with a condition
          variable, care should be taken to assure that one-time wakeups are not missed
          by the blocking thread.

          More formally, for any p and x such that p(x) is true, if a thread A calls
          sleep(p) and another thread B calls notify(x) and the call to sleep happens
          before the call to notify, all statements preceding the call to notify happens
          before any statements following the call to sleep.

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

        /** Block until an event mathcing the predicate is signaled. */
        void sleep(std::function<bool(event_type)> predicate);

        /** Signal the given event. */
        void notify(event_type event);

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
      unique_lock<mutex> lock(state->mutx);
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
  void wakeup_service<T>::notify(event_type event)
  {
    using thread::mutex;
    using thread::unique_lock;
    if (!state) throw error();
    /* Busy wait until all other threads have been woken. This could be replaced bv
       a secondary condition variable if the predicates takes up to much time.
       Note that even after the wait we may block on the mutex for the last predicate.
    */
    do {}
    while (state->pending > 0);
    {
      unique_lock<mutex> lock(state->mutx);
      state->event = event;
      int b = state->blocked;
      state->pending.store(b);
      state->condition.notify_all();
    }
  }
}

