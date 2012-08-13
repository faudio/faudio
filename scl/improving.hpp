
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
  // FIXME do we really need to pass the wakeup?
  static wakeup_service<std::nullptr_t> accumulator_static_wakeup;
  
  // Current state of an improving value, one of:
  //   imp_const    Constant value
  //   imp_acc      Still accumulating
  //   imp_unary    Improving value applied to a unary function 
  //   imp_binary   Improving values applied to a binary function
  template <class T>
  struct imp_state
  {
    virtual T    value() const = 0;
    virtual bool known() const = 0;
    virtual void wait()  const = 0;
  };
  
  template <class T> struct imp_state_ptr
  {
    using type = typename std::shared_ptr<imp_state<T>>;
  };

  template <class T>
  struct imp_const : public imp_state<T>
  {
    imp_const() = delete;
    explicit imp_const(T val) : val(val) {}

    T    value() const { return val; }
    bool known() const { return true; }
    void wait()  const { return; }
  private:
    T val;
  };

  template <class T>
  struct imp_acc : public imp_state<T>
  {        
    imp_acc() = delete;
    explicit imp_acc(std::shared_ptr<accumulator<T>> acc)
      : acc(acc) {}

    T    value() const { return acc->val; }
    bool known() const { return acc->val == acc->max; }
    void wait()  const 
    {
      std::function<bool(std::nullptr_t)> pred = 
        [this] (std::nullptr_t) { return this->known(); };
      accumulator_static_wakeup.sleep(pred);
    }
  private:
    std::shared_ptr<accumulator<T>> acc; // 8
  };

  template <class T>
  struct imp_unary : public imp_state<T>
  {
    imp_unary() = delete;
    imp_unary(
      std::function<T(T,T)>           func,
      typename imp_state_ptr<T>::type arg)
      : func(func),
        arg(arg) {}
    
    bool known() const 
    {
      return arg->known();
    }
    T value() const 
    {
      return func(arg->value());
    }
    void wait()  const 
    {
      return arg->wait();
    }
  private:
    std::function<T(T)>             func;             
    typename imp_state_ptr<T>::type arg;
  };
   

  template <class T>
  struct imp_binary : public imp_state<T>
  {
    imp_binary() = delete;
    imp_binary(
      std::function<T(T,T)>           func,
      typename imp_state_ptr<T>::type left,
      typename imp_state_ptr<T>::type right)
      : func(func),
        left(left),
        right(right) {}

    bool known() const 
    {
      bool lk = left->known();
      bool rk = right->known();
      return lk && rk;
    }
    T value() const 
    {
      T lv = left->value();
      T rv = right->value();
      return func(lv, rv);
    }
    void wait()  const 
    {
      std::function<bool(std::nullptr_t)> pred = 
        [this] (std::nullptr_t wakeup) { return this->known(); };
      accumulator_static_wakeup.sleep(pred);
    }
  private:
    std::function<T(T, T)> func;                 // 16
    typename imp_state_ptr<T>::type left;  // 8
    typename imp_state_ptr<T>::type right; // 8
  };
  
  
  template<typename T>
  typename imp_state_ptr<T>::type 
  min(const imp_state<T>& x, const imp_state<T>& y)
  {
    if (x.known() && x.value() <= y.value())
      return x;
    if (y.known() && y.value() <= x.value())
      return y;
    if (x.known() && y.known() && x.value() == y.value())
      return x;
  }
  template<typename T>
  typename imp_state_ptr<T>::type 
  max(const imp_state<T>& x, const imp_state<T>& y)
  {
    if (x.known() && x.value() <= y.value())
      return y;
    if (y.known() && y.value() <= x.value())
      return x;
    if (x.known() && y.known() && x.value() == y.value())
      return x;
  }

  template<typename T>
  typename imp_state_ptr<T>::type 
  operator+ (const imp_state<T>& x, const imp_state<T>& y)
  {
  }
  template<typename T>
  typename imp_state_ptr<T>::type 
  operator- (const imp_state<T>& x, const T& y)
  {
  }
  template<typename T>
  typename imp_state_ptr<T>::type 
  operator* (const imp_state<T>& x, const imp_state<T>& y)
  {
  }
  template<typename T>
  typename imp_state_ptr<T>::type 
  operator/ (const imp_state<T>& x, const T& y)
  {
  }

  template<typename T>
  typename imp_state_ptr<bool>::type 
  operator< (const imp_state<T>& x, const imp_state<T>& y)
  {
    if (x.known() && x.value() < y.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));

    if (y.known() && y.value() < x.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));

    if (x.known() && y.known() && x.value() == y.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));
  }
  template<typename T>
  typename imp_state_ptr<bool>::type 
  operator<= (const imp_state<T>& x, const imp_state<T>& y)
  {
    if (x.known() && x.value() <= y.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));

    if (x.known() && y.known() && x.value() == y.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));
  }
  template<typename T>
  typename imp_state_ptr<bool>::type 
  operator== (const imp_state<T>& x, const imp_state<T>& y)
  {
    if (x.known() && x.value() < y.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));

    if (y.known() && y.value() < x.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));

    if (x.known() && y.known() && x.value() == y.value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));
  }
  



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
    improving() noexcept
      : state(new imp_const<T>(std::numeric_limits<T>::lowest()))
    {}
    improving(const T value) noexcept
      : state(new imp_const<T>(value))
    {}
    improving(std::shared_ptr<accumulator<T>> acc) noexcept
      : state(new imp_acc<T>(acc))
    {}
    improving(this_type&& other)
      : state(std::move(other.state))
    {}
    ~improving() noexcept
    {}

    void operator =(this_type&& other)
    {
      this_type tmp = std::move(other);
      return tmp.swap(*this);
    }
    void swap(this_type& other)
    {
      std::swap(this->state, other.state);
    }

    bool known() const noexcept
    {  
      return state->known();
    }

    T value() const
    {
      return state->value();
    }
    void wait() const
    {
      state->wait();
    }

    thread::future<T> to_future() const;

  private:
    typename imp_state_ptr<T>::type state;
  };


  //  fixed
  //  min
  //  max
  //  <
  //  <=

  // template<typename T>
  // improving<T> min(const improving<T>& x, const improving<T>& y)
  // {
  //   if (x.known() && x.value() <= y.value())
  //     return x;
  //   if (y.known() && y.value() <= x.value())
  //     return y;
  //   if (x.known() && y.known() && x.value() == y.value())
  //     return x;
  // }
  // template<typename T>
  // improving<T> max(const improving<T>& x, const improving<T>& y)
  // {
  //   if (x.known() && x.value() <= y.value())
  //     return y;
  //   if (y.known() && y.value() <= x.value())
  //     return x;
  //   if (x.known() && y.known() && x.value() == y.value())
  //     return x;
  // }
  // 
  // template<typename T>
  // improving<T> operator+ (const improving<T>& x, const improving<T>& y);
  // template<typename T>
  // improving<T> operator- (const improving<T>& x, const T& y);
  // template<typename T>
  // improving<T> operator* (const improving<T>& x, const improving<T>& y);
  // template<typename T>
  // improving<T> operator/ (const improving<T>& x, const T& y)
  // {
  // }
  // 
  // template<typename T>
  // improving<bool> operator< (const improving<T>& x, const improving<T>& y)
  // {
  //   if (x.known() && x.value() < y.value())
  //     return true;
  //   if (y.known() && y.value() < x.value())
  //     return false;
  //   if (x.known() && y.known() && x.value() == y.value())
  //     return false;
  // }
  // template<typename T>
  // improving<bool> operator<= (const improving<T>& x, const improving<T>& y)
  // {
  //   if (x.known() && x.value() <= y.value())
  //     return true;
  //   // if (y.known() && y.value() <= x.value())
  //   //   return false;
  //   if (x.known() && y.known() && x.value() == y.value())
  //     return true;
  // }
  // template<typename T>
  // improving<bool> operator== (const improving<T>& x, const improving<T>& y)
  // {
  //   if (x.known() && x.value() < y.value())
  //     return false;
  //   if (y.known() && y.value() < x.value())
  //     return false;
  //   if (x.known() && y.known() && x.value() == y.value())
  //     return true;
  // }










  template <class T>
  class accumulator
  : public std::enable_shared_from_this<accumulator<T>>
  {
  public:
    using value_type = T;
    using this_type  = accumulator<T>;

    accumulator()
      : val(std::numeric_limits<T>::lowest()) 
      , max(std::numeric_limits<T>::max()) {}
    accumulator(value_type value)
      : val(val) 
      , max(std::numeric_limits<T>::max()) {}
    ~accumulator() = default;
    
    // improving<T> get_improving() const
    // {
    //   std::shared_ptr<accumulator<T>> acc (shared_from_this());
    //   return improving<T> (acc);
    // }
    
    // FIXME potential race here
    // use CAS in loop to set max
    void fix()
    {
      T val_ = val;
      T max_ = max;
      if (max.compare_exchange_strong(max_, val_))
        accumulator_static_wakeup.wake(nullptr);
    }
    void increment(const T& amount)
    { 
      // FIXME throw if called after fix
      val += amount;
      accumulator_static_wakeup.wake(nullptr);
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
