
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
    BOOST_CONCEPT_ASSERT((LowerBound<T>));
    BOOST_CONCEPT_ASSERT((LessThanComparable<T>));
    using value_type = T;

    improving() noexcept;        // Fixed at std::numeric_limits::lowest()
    improving(const T) noexcept; // Fixed at the given value
    ~improving() noexcept;

    bool known() const noexcept;
    T value() const;
    void wait() const;
    thread::future<T> to_future() const;

  private:
    atomic::atomic<T>    current;
    thread::future<void> computation;
  };

  // (<) :: improving a -> improving a -> improving a
  // x < y  is known to be true  iff x is known and x < y (as y can only grow)
  // x < y  is known to be false iff y is known and !(x < y) (as x can only grow)
  template <class T>
  bool operator <(const improving<T>& x, const improving<T>& y)
  {
    return false;
  }

//   int operator ==(const improving<T>& x, const improving<T>& y)
//   {
//   }
//   int operator +(const improving<T>& x, const improving<T>& y)
//   {
//   }
//   int operator -(const improving<T>& x, const improving<T>& y)
//   {
//   }
// 

// 
//   /**
//       The improving analogue to a promise.Can improve or fix the improving value, or set an
//       exception that will be transported to the reading thread.
//    */
//   template <class T>
//   class provider
//   {
//     get_improving();
// 
//     void fix();
//     void increment(const T& amount);
//     void increment_and_fix(const T& amount);
// 
//     void fix_at_thread_exit();
//     void increment_at_thread_exit(const T& amount);
//     void increment_and_fix_at_thread_exit(const T& amount);
// 
//     void set_exception(std::exception_ptr ptr);
//     void set_exception_at_thread_exit(std::exception_ptr ptr);
//   };
// 
//   // FIXME analogue to std::packaged_task
//   class packaged_integral
// {};

}
