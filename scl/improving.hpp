
#pragma once

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
  template <class T>
  class improving
  {
  public:
#ifdef DOXYGEN_SHOULD_SKIP_THIS
    BOOST_CONCEPT_ASSERT((LowerBound<T>));
    BOOST_CONCEPT_ASSERT((LessThanComparable<T>));
#endif
    using value_type = T;
    using this_type  = improving<T>;

    improving() noexcept;        // Fixed at std::numeric_limits::lowest()
    improving(const T) noexcept; // Fixed at the given value
    improving(this_type &&);
    ~improving() noexcept;

    void operator =(improving &&);
    void swap(this_type& other);

    bool known() const noexcept;
    T value() const;
    void wait() const;
    thread::future<T> to_future() const;

  private:
    atomic::atomic<T> current;
    atomic::atomic<T> max;
    // cond var
    // provider
  };

  template <class T>
  improving<T>::improving()
    : current(std::numeric_limits<T>::lowest()) {}

  /*
    wait()
    {
      block on cond var
       wake up if (current > max)
    }

    known()
    {
      current.get() == max.get();
    }

    min(x, y)
    {
      return improving(
        max(this->current, other.current),
        max(this->max, other.max)
        provider
        );
    }
  */



  /**
      The improving analogue to a promise.Can improve or fix the improving value, or set an
      exception that will be transported to the reading thread.
   */
  template <class T>
  class provider
  {
    improving<T> get_improving();

    void fix();
    void increment(const T& amount);
    void increment_and_fix(const T& amount);

    void fix_at_thread_exit();
    void increment_at_thread_exit(const T& amount);
    void increment_and_fix_at_thread_exit(const T& amount);

    void set_exception(std::exception_ptr ptr);
    void set_exception_at_thread_exit(std::exception_ptr ptr);

  private:
    // list of improving
  };

  // // increment increments atomic var
  // fix ()
  // {
  //   // may only be called once
  //   // signal on first cond var
  // }
  // inc ()
  // {
  //   // add to atomic var
  //   // signal on first cond var
  // }

}
