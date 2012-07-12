/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

/**
    \file       util/atomic.h
    \brief      Cross-platform atomic operations.

    Set backend by defining one of the following:
      * SCL_ATOMIC_WIN
      * SCL_ATOMIC_OSX
      * SCL_ATOMIC_GCC

 */

#ifndef _SCLAUDIOX_UTIL_ATOMIC
#define _SCLAUDIOX_UTIL_ATOMIC

#include <boost/type_traits.hpp>

#include "sclaudiox/error.h"
#include "sclaudiox/util/misc.h"

#ifdef SCL_WIN
    #define SCL_ATOMIC_WIN
#endif
#ifdef SCL_OSX
    #define SCL_ATOMIC_GCC
#endif


#ifdef SCL_ATOMIC_WIN
    #include "Windows.h"
#endif

#ifdef SCL_ATOMIC_OSX
    #include <libkern/OSAtomic.h>
#endif


namespace doremir {
namespace scl {

    /**
        This class provides low-level atomic operations.
        It is the standard backend for Atomic.
     */
    template <class T>
    struct default_atomic_traits
    {
        static T    get(T* a);
        static void add(T* a, T x);
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


/**
    
  */
template <
    typename T, 
    typename atomic_traits = default_atomic_traits<T>
>
class SCLAUDIO_API atomic
{
public:   
    typedef          atomic<T, atomic_traits>      this_type;
    typedef typename boost::remove_const<T>::type  value_type;
            
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
    template <
        class U
    > 
    explicit atomic(U value) : mData(value) {}

    /**
        Copy constructor.
        Creates a new atomic value by copying the value of the given atomic variable.
        \note
            Non-atomic operation.
     */
    atomic(const atomic<T>& other) : mData(other.value()) {}

    /**
        Destroys the atomic value.
        \note
            Non-atomic operation.
     */
    virtual ~atomic() {}   

    /**
        Assignment operator.
        Replaces the current value with the value of the given atomic variable.
        \note
            Atomic operation.
     */
    void operator =(const atomic<T>& other)
    {
        replace(other.value());
    }
    
    /**
        Retreive the current value.
        \note
            Atomic operation.
     */
    inline T value() const
    {
        return atomic_traits::get(const_cast<value_type*>(&mData));
    }
    
    /**
        Replace the current value with the given value.
        \note
            Atomic operation.
     */
    void replace(T newVal)
    {
        T oldVal;
        do 
            oldVal = atomic_traits::get(&mData); 
        while (!try_replace(oldVal, newVal));
    }
    
    /**
        Replace the current value with the given new value, if the given old value 
        equals the current value.
        \note
            Atomic operation.
     */
    inline bool try_replace(T oldVal, T newVal)
    {
        return atomic_traits::swap(&mData, oldVal, newVal);
    }  

    /**
        Update the current value by applying the given function.

        \invariant
            The given function should be free of side-effects.
        \note
            Atomic operation.
     */
    void modify(Function<T,T> function)
    {      
        while(!try_modify(function));
    }

    /**
        Attempt to update the current value by applying the given function.

        If the value was modified, this method returns `true`.
        
        If the current value changed while the new value was being computed, no modification
        occurs and this method returns `false`.
        
        \note
            Atomic operation.
     */
    inline bool try_modify(Function<T,T> function)
    {
        T oldVal, newVal;
        oldVal = atomic_traits::get(&mData);
        newVal = function(oldVal);
        return atomic_traits::swap(&mData, oldVal, newVal);
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
        atomic temp (this);
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
        atomic temp (this);
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
        return atomic_traits::add(&mData, amount);
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
    return a.value() == b.value();
}  

template <class T>
bool operator !=(const atomic<T>& a, const atomic<T>& b)
{
    return a.value() != b.value();	    
}

typedef atomic<char>            atomic_char;
typedef atomic<unsigned char>   atomic_uchar;
typedef atomic<signed char>     atomic_schar;
typedef atomic<unsigned short>  atomic_ushort;
typedef atomic<short>           atomic_short;
typedef atomic<unsigned int>    atomic_uint;
typedef atomic<int>             atomic_int;
typedef atomic<unsigned long>   atomic_ulong;
typedef atomic<long>            atomic_long;
typedef atomic<uint8_t>         atomic_uint8_t;
typedef atomic<int8_t>          atomic_int8_t;
typedef atomic<uint16_t>        atomic_uint16_t;
typedef atomic<int16_t>         atomic_int16_t;
typedef atomic<uint32_t>        atomic_uint32_t;
typedef atomic<int32_t>         atomic_int32_t;
typedef atomic<uint64_t>        atomic_uint64_t;
typedef atomic<int64_t>         atomic_int64_t;
typedef atomic<void*>           atomic_address;
typedef atomic<bool>            atomic_bool;

} // namespace
} // namespace

#endif
