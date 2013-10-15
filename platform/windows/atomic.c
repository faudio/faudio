
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/atomic.h>
#include <Windows.h>

/*
    TODO these are strictly 32-bit for now
 */
struct _fa_atomic_t {
    impl_t      impl;       //  Interface dispatcher
    intptr_t    value;
};

fa_atomic_t fa_atomic_create()
{
    fa_atomic_t a = malloc(sizeof(struct _fa_atomic_t));
    return a;
}

fa_atomic_t fa_atomic_copy(fa_atomic_t a)
{
    free(a); // This is free
}

void fa_atomic_swap(fa_atomic_t a, fa_atomic_t b)
{
}

void fa_atomic_destroy(fa_atomic_t a)
{
}

bool fa_atomic_exchange(fa_atomic_t a, fa_ptr_t pold, fa_ptr_t pnew)
{
}

void fa_atomic_add(fa_atomic_t a, intptr_t v)
{
}

void* fa_atomic_get(fa_atomic_t a)
{
}

void fa_atomic_modify(fa_atomic_t atomic, fa_unary_t func, fa_ptr_t data)
{
}

void fa_atomic_set(fa_atomic_t a, fa_ptr_t v)
{
}




/*
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
*/


