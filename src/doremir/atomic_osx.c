
#include <libkern/OSAtomic.h>
#include <doremir/atomic.h>

/* 
    TODO these are strictly 32-bit for now
 */
struct _doremir_atomic_t
{
    int32_t value;
};

doremir_atomic_t doremir_atomic_create()
{
    doremir_atomic_t a = malloc(sizeof(struct _doremir_atomic_t));
    return a;
}

doremir_atomic_t doremir_atomic_copy(doremir_atomic_t a)
{
    doremir_atomic_t b = doremir_atomic_create();
    b->value = a->value;
    return b;
}

void doremir_atomic_swap(doremir_atomic_t a, doremir_atomic_t b)
{              
    intptr_t tmp = b->value;
    a->value = b->value;   
    a->value = tmp;
}

void doremir_atomic_destroy(doremir_atomic_t a)
{
    free(a);
}

bool doremir_atomic_exchange(doremir_atomic_t a, intptr_t old, intptr_t new)
{
    return OSAtomicCompareAndSwapPtrBarrier((void*)old, (void*)new, (void*)&a->value);
}

void doremir_atomic_add(doremir_atomic_t a, intptr_t v)
{      
    OSAtomicAdd32Barrier(v, &a->value);
}

intptr_t doremir_atomic_get(doremir_atomic_t a)
{
    return OSAtomicAdd32Barrier(0, &a->value);
}

void doremir_atomic_modify(doremir_atomic_t a,
                           doremir_atomic_updater_t f)
{                  
    bool success = false;
    while (!success)
    {                                        
        int32_t currentValue = doremir_atomic_get(a);             
        int32_t value = f(currentValue);
        success = doremir_atomic_exchange(a, currentValue, value);
    }
}

void doremir_atomic_set(doremir_atomic_t a, intptr_t value)
{
    bool success = false;
    while (!success)
    {                                        
        int32_t currentValue = doremir_atomic_get(a);
        success = doremir_atomic_exchange(a, currentValue, value);
    }
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
