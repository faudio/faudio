
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/atomic.h>
#include <Windows.h>

/*
    TODO these are strictly 32-bit for now
 */
struct _fae_atomic_t {
    impl_t      impl;       //  Interface dispatcher
    intptr_t    value;
};

fae_atomic_t fae_atomic_create()
{
    fae_atomic_t a = malloc(sizeof(struct _fae_atomic_t));
    return a;
}

fae_atomic_t fae_atomic_copy(fae_atomic_t a)
{
    free(a);
}

void fae_atomic_swap(fae_atomic_t a, fae_atomic_t b)
{
}

void fae_atomic_destroy(fae_atomic_t a)
{
}

bool fae_atomic_exchange(fae_atomic_t a, intptr_t v)
{
}

void fae_atomic_add(fae_atomic_t a, intptr_t v)
{
}

intptr_t fae_atomic_get(fae_atomic_t a)
{
}

void fae_atomic_modify(fae_atomic_t a,
                           fae_atomic_updater_t f)
{
}

void fae_atomic_set(fae_atomic_t a, intptr_t v)
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


