
#ifndef _ATOMIC_IMPL
#define _ATOMIC_IMPL


#ifdef _WIN32
  #define SCL_ATOMIC_WIN
#else
  #define SCL_ATOMIC_GCC
#endif

#ifdef SCL_ATOMIC_WIN
  #include "Windows.h"
#endif

#ifdef SCL_ATOMIC_OSX
  #include <libkern/OSAtomic.h>
#endif

/** This class provides low-level atomic operations.
    It is the standard backend for Atomic.
 */
template <class T>
struct default_atomic_traits
{
  static T    get (T* a);
  static void add (T* a, T x);
  static bool swap(T* a, T x, T y);
};
  
#ifdef SCL_ATOMIC_OSX
  template <>
  struct default_atomic_traits<int32_t>
  {
    inline static bool swap(int32_t* a, int32_t x, int32_t y)
    {
      return OSAtomicCompareAndSwap32Barrier(x, y, a);
    }
    static int32_t get(int32_t* a)
    {
      return OSAtomicAdd32Barrier(0, a);
    }
    static void add(int32_t* a, int32_t x)
    {
      OSAtomicAdd32Barrier(x, a);
    }
  };

  template <class T>
  struct default_atomic_traits<T*>
  {
    static bool swap(T** a, T* x, T* y)
    {
      return OSAtomicCompareAndSwapPtrBarrier((void*) x, (void*) y, (void**) a);
    }
    static T* get(T** a)
    {
      return *a;
    }
  };
#endif


#ifdef SCL_ATOMIC_WIN
  template <>
  struct default_atomic_traits<int32_t>
  {
    static bool swap(int32_t* a, int32_t x, int32_t y)
    {
      return ( x == InterlockedCompareExchange((LONG*) a, (LONG) y, (LONG) x) );
    }
    static int32_t get(int32_t* a)
    {
      return InterlockedCompareExchange((LONG*) a, (LONG) 0, (LONG) 0);
    }
    static void add(int32_t* a, int32_t x)
    {
      InterlockedExchangeAdd((LONG*) a, (LONG) x);
    }
  };

  template <class T>
  struct default_atomic_traits<T*>
  {
    static bool swap(T** a, T* x, T* y)
    {
      return ( x == InterlockedCompareExchangePointer((void**) a, (void*) y, (void*) x) );
    }
    static T* get(T** a)
    {
      return reinterpret_cast<T*>(InterlockedCompareExchangePointer((void**) a, (void*) 0, (void*) 0));
    }
  };
#endif

#ifdef SCL_ATOMIC_GCC
  template <>
  struct default_atomic_traits<int32_t>
  {
    static bool swap(int32_t* a, int32_t x, int32_t y)
    {
      return __sync_bool_compare_and_swap(a, x, y);
    }
    static int32_t get(int32_t* a)
    {
      return __sync_val_compare_and_swap(a, 0, 0);
    }
    static void add(int32_t* a, int32_t x)
    {
      __sync_fetch_and_add (a, x);
    }
  };

  template <class T>
  struct default_atomic_traits<T*>
  {
    static bool swap(T** a, T* x, T* y)
    {
      return __sync_bool_compare_and_swap(a, x, y);
    }
    static T* get(T** a)
    {
      return __sync_val_compare_and_swap(a, 0, 0);
    }
  };
#endif

#endif