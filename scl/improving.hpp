
#pragma once

#include <memory>
#include <scl/atomic.hpp>
#include <scl/thread.hpp>
#include <scl/concept.hpp>
#include <scl/future/wakeup_service.hpp>

namespace scl
{
  template <class T> class improving;
  template <class T> class accumulator;






  template <class T>
  class improving
  {
  private:
    BOOST_CONCEPT_ASSERT((LowerBound<T>));
    BOOST_CONCEPT_ASSERT((LessThanComparable<T>));
    using accumulator_type = accumulator<T>;
  public:
    using value_type = T;
    using this_type  = improving<T>;
  public:
    improving() noexcept;        // Fixed at std::numeric_limits::lowest()
    improving(const T) noexcept; // Fixed at the given value
    improving(this_type &&);
    ~improving() noexcept;

    void operator =(improving &&);
    void swap(this_type& other);

    bool known() const noexcept
    {
      // if single, true
      // if accumulator, return val == max
      // if binary, OP(val1, val2) == OP(max1, max2)       always OK ??????
    }
    T value() const
    {
      // if single, the value
      // if accumulator, val
      // if binary, OP(val1, val2)
    }
    void wait() const
    {
      // if single, return
      // if otherwise, block until known()
    }

    thread::future<T> to_future() const;

  private:
    T val; // 4
    std::shared_ptr<accumulator<T>> acc; // 8

    std::function<T(T,T)>         binary_op; // 16
    std::shared_ptr<improving<T>> acc_left; // 8
    std::shared_ptr<improving<T>> acc_right; // 8
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
