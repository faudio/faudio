
#pragma once

#include <memory>
#include <scl/atomic.hpp>
#include <scl/thread.hpp>
#include <scl/concept.hpp>
#include <scl/future/wakeup_service.hpp>

namespace scl
{

  template <class T>
  class accumulator;

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
  private:
    using accumulator_type = accumulator<T>;
  public:
    improving() noexcept;        // Fixed at std::numeric_limits::lowest()
    improving(const T) noexcept; // Fixed at the given value
    improving(this_type &&);
    ~improving() noexcept;

    void operator =(improving &&);
    void swap(this_type& other);

    bool known() const noexcept
    {
    }
    T value() const
    {
    }
    void wait() const
    {
    }

    thread::future<T> to_future() const;

  private:
    bool is_compound;
    std::function<T(T,T)>             binary_op;
    std::shared_ptr<accumulator_type> acc_left;
    std::shared_ptr<accumulator_type> acc_right;
  };
  
  //  fixed
  //  min
  //  max
  //  <
  //  <=

  template<typename T>
  improving<T> min(const improving<T>& a, const improving<T>& b);
  template<typename T>
  improving<T> max(const improving<T>& a, const improving<T>& b);

  template<typename T>
  improving<T> operator+ (const improving<T> &a, const improving<T> &b);
  template<typename T>
  improving<T> operator- (const improving<T> &a, const T &b);
  template<typename T>
  improving<T> operator* (const improving<T> &a, const improving<T> &b);
  template<typename T>
  improving<T> operator/ (const improving<T> &a, const T &b);

  template<typename T>
  improving<bool> operator< (const improving<T> &a, const improving<T> &b);
  template<typename T>
  improving<bool> operator<= (const improving<T> &a, const improving<T> &b);




  static wakeup_service<intptr_t> accumulator_static_wakeup;

  /**
      The improving analogue to a promise.Can improve or fix the improving value, or set an
      exception that will be transported to the reading thread.
   */
  template <class T>
  class accumulator
  {
  public:
    improving<T> get_improving() const;
  
    void fix()
    {
      T x = val;
      max = x;    
      intptr_t p = reinterpret_cast<intptr_t>(this);
      accumulator_static_wakeup.wake(p);
    }
    void increment(const T& amount)
    {
      val += amount;
      intptr_t p = reinterpret_cast<intptr_t>(this);
      accumulator_static_wakeup.wake(p);
    }
    // void increment_and_fix(const T& amount)
    // {
    //   increment(amount);
    //   fix();
    // }
  // private:
    atomic::atomic<T> val;
    atomic::atomic<T> max;
  }; 


}
