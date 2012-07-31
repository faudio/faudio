
#ifndef _ATOMIC
#define _ATOMIC

#define SCLAUDIO_API

#include "atomic_impl.h"

namespace math
{
  template <class T>
  T negate(T x)
  {
    return x * -1;
  }
}

/** Implements atomics as specified in the C++0x standard 29.5, with the following limitations:

      * No exchange,         see try_replace
      * No compare_exchange, see try_replace
  */
template < 
  class T, 
  class AtomicTraits = default_atomic_traits<T>
>
class SCLAUDIO_API atomic
{
public:
  using traits_type = AtomicTraits;
  using value_type  = typename std::remove_const<T>::type;
  using this_type   = atomic<T, traits_type>;

  /**
    Constructs an atomic value using the nullary constructor `T()`.
    \note
      Non-atomic operation.
   */
  atomic() {}

  /**
    Constructs an atomic value using a unary constructor `T(U x)`.
    \note
      Non-atomic operation.
   */
  template <class U> explicit
  atomic(U value) : mData(value) {}

  /**
    Copy constructor.
    Creates a new atomic value by copying the value of the given atomic variable.
    \note
      Non-atomic operation.
   */
  atomic(const atomic<T>& other) : mData(other.load()) {}

  /**
    Destroys the atomic value.
    \note
      Non-atomic operation.
   */
  ~atomic() {}

  bool is_lock_free() const volatile
  {
    return true;
  }

  bool is_lock_free() const
  {
    return true;
  }

  /**
    Assignment operator.
    Replaces the current value with the value of the given atomic variable.
    \note
      Atomic operation.
   */
  void operator =(const atomic<T>& other)
  {
    store(other.load());
  }

  operator T() const volatile
  {
    return load();
  }

  operator T() const
  {
    return load();
  }


  /**
    Retreive the current value.
    \note
      Atomic operation.
   */
  inline T load() const volatile
  {
    return traits_type::get(const_cast<value_type*>(&mData));
  }

  inline T load() const
  {
    return traits_type::get(const_cast<value_type*>(&mData));
  }

  /**
    Replace the current value with the given value.
    \note
      Atomic operation.
   */
  void store(T y) const volatile
  {
    T x;
    do
      x = traits_type::get(const_cast<value_type*>(&mData));
    while (!try_replace(x, y));
  }

  void store(T y) const
  {
    T x;
    do
      x = traits_type::get(const_cast<value_type*>(&mData));
    while (!try_replace(x, y));
  }

  /**
    Replace the current value with the given new value, if the given old value
    equals the current value.
    \note
      Atomic operation.
   */
  inline bool try_replace(T x, T y) const
  {
    return traits_type::swap(const_cast<value_type*>(&mData), x, y);
  }

  /**
    Update the current value by applying the given function.

    \invariant
      The given function should be free of side-effects.
    \note
      Atomic operation.
   */
  void update(std::function<T(T)> function) const
  {
    while (!try_update(function));
  }

  /**
    Attempt to update the current value by applying the given function.

    If the value was modified, this method returns `true`.

    If the current value changed while the new value was being computed, no modification
    occurs and this method returns `false`.

    \note
      Atomic operation.
   */
  inline bool try_update(std::function<T(T)> function) const
  {
    T x, y;
    x = traits_type::get(const_cast<value_type*>(&mData));
    y = function(x);
    return traits_type::swap(const_cast<value_type*>(&mData), x, y);
  }

  /**
    Prefix increment operator.
    \note
      Atomic operation.
   */
  inline this_type& operator ++()
  {
    increment(1);
    return *this;
  }

  /**
    Prefix decrement operator.
    \note
      Atomic operation.
   */
  inline this_type& operator --()
  {
    decrement(1);
    return *this;
  }

  /**
    Postfix increment operator.
    \note
      Atomic operation.
   */
  inline this_type operator ++(int post)
  {
    atomic temp(this);
    increment(1);
    return temp;
  }

  /**
    Postfix decrement operator.
    \note
      Atomic operation.
   */
  inline this_type operator --(int post)
  {
    atomic temp(this);
    decrement(1);
    return temp;
  }

  /**
    Increase the atomic number by the given amount.
    \note
      Atomic operation.
   */
  inline void increment(T amount)
  {
    return traits_type::add(&mData, amount);
  }

  /**
    Decrease the atomic number by the given amount.
    \note
      Atomic operation.
   */
  inline void decrement(T amount)
  {
    return increment(math::negate(amount));
  }

private:
  value_type mData;
};

template <class T>
bool operator ==(const atomic<T>& a, const atomic<T>& b)
{
  return a.load() == b.load();
}

template <class T>
bool operator !=(const atomic<T>& a, const atomic<T>& b)
{
  return a.load() != b.load();
}

using atomic_char               = atomic<char>;
using atomic_schar              = atomic<signed char>;
using atomic_uchar              = atomic<unsigned char>;
using atomic_short              = atomic<short>;
using atomic_ushort             = atomic<unsigned short>;
using atomic_int                = atomic<int>;
using atomic_uint               = atomic<unsigned int>;
using atomic_long               = atomic<long>;
using atomic_ulong              = atomic<unsigned long>;
using atomic_llong              = atomic<long long>;
using atomic_ullong             = atomic<unsigned long long>;
using atomic_char16_t           = atomic<char16_t>;
using atomic_char32_t           = atomic<char32_t>;
using atomic_wchar_t            = atomic<wchar_t>;
using atomic_int_least8_t       = atomic<int_least8_t>;
using atomic_uint_least8_t      = atomic<uint_least8_t>;
using atomic_int_least16_t      = atomic<int_least16_t>;
using atomic_uint_least16_t     = atomic<uint_least16_t>;
using atomic_int_least32_t      = atomic<int_least32_t>;
using atomic_uint_least32_t     = atomic<uint_least32_t>;
using atomic_int_least64_t      = atomic<int_least64_t>;
using atomic_uint_least64_t     = atomic<uint_least64_t>;
using atomic_int_fast8_t        = atomic<int_fast8_t>;
using atomic_uint_fast8_t       = atomic<uint_fast8_t>;
using atomic_int_fast16_t       = atomic<int_fast16_t>;
using atomic_uint_fast16_t      = atomic<uint_fast16_t>;
using atomic_int_fast32_t       = atomic<int_fast32_t>;
using atomic_uint_fast32_t      = atomic<uint_fast32_t>;
using atomic_int_fast64_t       = atomic<int_fast64_t>;
using atomic_uint_fast64_t      = atomic<uint_fast64_t>;
using atomic_intptr_t           = atomic<intptr_t>;
using atomic_uintptr_t          = atomic<uintptr_t>;
using atomic_size_t             = atomic<size_t>;
using atomic_ptrdiff_t          = atomic<ptrdiff_t>;
using atomic_intmax_t           = atomic<intmax_t>;
using atomic_uintmax_t          = atomic<uintmax_t>;


#endif
