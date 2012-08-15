
#pragma once

#include <memory>
#include <scl/atomic.hpp>
#include <scl/thread.hpp>
#include <scl/concept.hpp>
#include <scl/future/wakeup_service.hpp>

#define SCL_IMPROVING_OPTIMIZE

namespace scl
{
  template <class T> class improving;
  template <class T> class accumulator;

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
    imp_state_ptr() = delete;
    using type = typename std::shared_ptr<imp_state<T>>;
  };

  template <class T>
  struct imp_const : public imp_state<T>
  {
    imp_const() = delete;
    explicit imp_const(T val) : val(val) {}

    T value() const
    {
      return val;
    }
    bool known() const
    {
      return true;
    }
    void wait()  const
    {
      return;
    }
  private:
    T val;
  };

  template <class T>
  struct imp_acc : public imp_state<T>
  {
    imp_acc() = delete;
    explicit imp_acc(std::shared_ptr<accumulator<T>> acc)
      : acc(acc) {}

    T value() const
    {
      return acc->val;
    }
    bool known() const
    {
      return acc->val >= acc->max;
    }
    void wait()  const
    {
      std::function<bool(std::nullptr_t)> pred =
        [this](std::nullptr_t)
      {
        return this->known();
      };
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
      std::function<T(T, T)>          func,
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
      std::function<T(T, T)>          func,
      typename imp_state_ptr<T>::type left,
      typename imp_state_ptr<T>::type right)
      : func(func),
        left(left),
        right(right) {}

    bool known() const
    {
      if (left->known())
      {
        if (right->known())
          return true;
        if (left->value() <= right->value())
          return true;
      }
      if (right->known())
      {
        if (right->value() <= left->value())
          return true;
      }
      return false;
    }
    T value() const
    {
      return func(left->value(), right->value());
    }
    void wait()  const
    {
      std::function<bool(std::nullptr_t)> pred =
        [this](std::nullptr_t wakeup)
      {
        return this->known();
      };
      accumulator_static_wakeup.sleep(pred);
    }
  private:
    std::function<T(T, T)>          func;
    typename imp_state_ptr<T>::type left;
    typename imp_state_ptr<T>::type right;
  };


  template<typename T>
  typename imp_state_ptr<T>::type
  min(typename imp_state_ptr<T>::type x,
      typename imp_state_ptr<T>::type y)
  {
#ifdef SCL_IMPROVING_OPTIMIZE
    if (x->known() && x->value() <= y->value())
      return x;

    if (y->known() && y->value() <= x->value())
      return y;

    if (x->known() && y->known() && x->value() == y->value())
      return x;
#endif // SCL_IMPROVING_OPTIMIZE

    return typename imp_state_ptr<T>::type(
             new imp_binary<T>(
               [](T x, T y) -> T { return std::min(x, y); },
               x, y));
  }

  template<typename T>
  typename imp_state_ptr<T>::type
  max(typename imp_state_ptr<T>::type x,
      typename imp_state_ptr<T>::type y)
  {
#ifdef SCL_IMPROVING_OPTIMIZE
    if (x->known() && x->value() <= y->value())
      return y;
    
    if (y->known() && y->value() <= x->value())
      return x;
    
    if (x->known() && y->known() && x->value() == y->value())
      return x;
#endif // SCL_IMPROVING_OPTIMIZE

    return typename imp_state_ptr<T>::type(
             new imp_binary<T>(
               [](T x, T y) -> T { return std::max(x, y); },
               x, y));
  }

  template<typename T>
  typename imp_state_ptr<T>::type
  operator+ (typename imp_state_ptr<T>::type x,
             typename imp_state_ptr<T>::type y)
  {
    return typename imp_state_ptr<T>::type(
             new imp_binary<T>(
               std::plus<T>(),
               x, y));
  }

  template<typename T>
  typename imp_state_ptr<T>::type
  operator- (typename imp_state_ptr<T>::type x,
             typename imp_state_ptr<T>::type y)
  {
    // FIXME
  }

  template<typename T>
  typename imp_state_ptr<T>::type
  operator* (typename imp_state_ptr<T>::type x,
             typename imp_state_ptr<T>::type y)
  {
    return typename imp_state_ptr<T>::type(
             new imp_binary<T>(
               std::multiplies<T>(),
               x, y));
  }

  template<typename T>
  typename imp_state_ptr<T>::type
  operator/ (typename imp_state_ptr<T>::type x,
             typename imp_state_ptr<T>::type y)
  {
    // FIXME
  }

  template<typename T>
  typename imp_state_ptr<bool>::type
  operator< (typename imp_state_ptr<T>::type x,
             typename imp_state_ptr<T>::type y)
  {
#ifdef SCL_IMPROVING_OPTIMIZE
    if (x->known() && x->value() < y->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));

    if (y->known() && y->value() < x->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));

    if (x->known() && y->known() && x->value() == y->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));
#endif // SCL_IMPROVING_OPTIMIZE

    return typename imp_state_ptr<bool>::type(
             new imp_binary<T>(
               std::less<T>(),
               x, y));
  }

  template<typename T>
  typename imp_state_ptr<bool>::type
  operator<= (typename imp_state_ptr<T>::type x,
              typename imp_state_ptr<T>::type y)
  {
#ifdef SCL_IMPROVING_OPTIMIZE
    if (x->known() && x->value() <= y->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));

    if (x->known() && y->known() && x->value() == y->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));
#endif // SCL_IMPROVING_OPTIMIZE

    return typename imp_state_ptr<bool>::type(
             new imp_binary<T>(
               std::less_equal<T>(),
               x, y));
  }

  template<typename T>
  typename imp_state_ptr<bool>::type
  operator== (typename imp_state_ptr<T>::type x,
              typename imp_state_ptr<T>::type y)
  {
#ifdef SCL_IMPROVING_OPTIMIZE
    if (x->known() && x->value() < y->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));

    if (y->known() && y->value() < x->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(false));

    if (x->known() && y->known() && x->value() == y->value())
      return imp_state_ptr<bool>::type(new imp_const<bool>(true));
#endif // SCL_IMPROVING_OPTIMIZE

    return typename imp_state_ptr<bool>::type(
             new imp_binary<T>(
               std::equal<T>(),
               x, y));
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
  :
    state(new imp_const<T>(std::numeric_limits<T>::lowest()))
    {}
    explicit
    improving(const T value) noexcept
  :
    state(new imp_const<T>(value))
    {}
    explicit
    improving(std::shared_ptr<accumulator<T>> acc) noexcept
  :
    state(new imp_acc<T>(acc))
    {}
    improving(this_type && other)
      : state(std::move(other.state))
    {}
    ~improving() noexcept
    {}
  public: // FIXME make private after friending ops
    improving(typename imp_state_ptr<T>::type state)
      : state(state) {}
  public:

    void operator= (this_type && other)
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

	// FIXME
    // thread::future<T> to_future() const;

    // FIXME friend the operators
    // private:
    typename imp_state_ptr<T>::type state;
  };


  template<typename T>
  improving<T> min(const improving<T>& x, const improving<T>& y)
  {
    return improving<T>(
             scl::min<T>(x.state, y.state));
  }

  template<typename T>
  improving<T> max(const improving<T>& x, const improving<T>& y)
  {
    return improving<T>(
             scl::max<T>(x.state, y.state));
  }

  template<typename T>
  improving<T> operator+ (const improving<T>& x, const improving<T>& y)
  {
    return improving<T>(
             scl::operator+ <T>(x.state, y.state));
  }

  template<typename T>
  improving<T> operator- (const improving<T>& x, const T& y)
  {
    // FIXME
  }

  template<typename T>
  improving<T> operator* (const improving<T>& x, const improving<T>& y)
  {
    return improving<T>(
             scl::operator* <T>(x.state, y.state));
  }

  template<typename T>
  improving<T> operator/ (const improving<T>& x, const T& y)
  {
    // FIXME
  }

  template<typename T>
  improving<bool> operator< (const improving<T>& x, const improving<T>& y)
  {
    return improving<bool>(
             scl::operator< <T>(x.state, y.state));
  }
  template<typename T>
  improving<bool> operator<= (const improving<T>& x, const improving<T>& y)
  {
    return improving<bool>(
             scl::operator<= <T>(x.state, y.state));
  }
  template<typename T>
  improving<bool> operator== (const improving<T>& x, const improving<T>& y)
  {
    return improving<bool>(
             scl::operator== <T>(x.state, y.state));
  }

  template <class T>
  std::ostream& operator<< (std::ostream& out, const improving<T>& x)
  {
    out << (x.known() ? "@{" : "~{")
        << x.value()
        << (x.known() ? "}" : "}");
    return out;
  }






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
      : val(value)
      , max(std::numeric_limits<T>::max()) {}
    ~accumulator() = default;

    improving<T> get_improving() // FIXME const
    {                         
      return improving<T> (
        std::enable_shared_from_this<this_type>
          ::shared_from_this());
    }

    void fix()
    {
      T val_ = val;
      T max_ = max;
      if (max.compare_exchange_strong(max_, val_))
        accumulator_static_wakeup.wake(nullptr);
    }
    void increment(const T& amount)
    {
      T x;
      do
      {
        x = val;
      }
      while (!val.compare_exchange_strong(x, x + amount));
      accumulator_static_wakeup.wake(nullptr);
    }
    void increment_and_fix(const T& amount)
    {
      // FIXME
    }

    // private:
    atomic::atomic<T> val;
    atomic::atomic<T> max;
  };

}
