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

#include "sclaudiox/error.h"
#include "sclaudiox/numeric.h"
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
    struct DefaultAtomicTraits
    {
        static T    get(T* a);
        static void add(T* a, T x);
        static bool swap(T* a, T x, T y);
    };
    
#ifdef SCL_ATOMIC_OSX
    template <>
    struct DefaultAtomicTraits<Int32>
    {
        inline static bool swap(Int32* a, Int32 x, Int32 y)
        {
            return OSAtomicCompareAndSwap32Barrier(x, y, a);
        }
        static Int32 get(Int32* a)
        {
            return OSAtomicAdd32Barrier(0, a);
        }
        static void add(Int32* a, Int32 x)
        {
            OSAtomicAdd32Barrier(x, a);
        }
    };

    template <class T>
    struct DefaultAtomicTraits<T*>
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
    struct DefaultAtomicTraits<Int32>
    {
        static bool swap(Int32* a, Int32 x, Int32 y)
        {
            return ( x == InterlockedCompareExchange((LONG*) a, (LONG) y, (LONG) x) );
        }
        static Int32 get(Int32* a)
        {
            return InterlockedCompareExchange((LONG*) a, (LONG) 0, (LONG) 0);
        }
        static void add(Int32* a, Int32 x)
        {
            InterlockedExchangeAdd((LONG*) a, (LONG) x);
        }
    };

    template <class T>
    struct DefaultAtomicTraits<T*>
    {
        static bool swap(T** a, T* x, T* y)
        {
            return ( comparand == InterlockedCompareExchangePointer((void**) a, (void*) y, (void*) x) );
        }
        static T* get(T** a)
        {
            return InterlockedCompareExchangePointer((void**) a, (void*) 0, (void*) 0);
        }
    };
#endif

#ifdef SCL_ATOMIC_GCC
    template <>
    struct DefaultAtomicTraits<Int32>
    {
        static bool swap(Int32* a, Int32 x, Int32 y)
        {
            return __sync_bool_compare_and_swap(a, x, y);
        }
        static Int32 get(Int32* a)
        {
            return __sync_val_compare_and_swap(a, 0, 0);
        }
        static void add(Int32* a, Int32 x)
        {
            __sync_fetch_and_add (a, x);
        }
    };

    template <class T>
    struct DefaultAtomicTraits<T*>
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
    An atomic value.
    
    The value() and tryReplace() methods are usually implemented through compiler intrisics or
    platform-specific functions. replace() is implemented in terms of tryReplace() using
    a spin-lock.
    
    Atomics should be through of as special variables, rather than as values.
    In the following example integer variables are replaced by atomic integer variables:
    
        int foo = 1;
        int bar = 2;
        bar = foo;
        
        Atomic<int> foo = 1;
        Atomic<int> bar = 2;
        bar = foo;

    \invariant
        T is supported by the underlying AtomicTraits. 
        Using the default traits this means `int`, `unsigned int` and pointers.
  */
template <
    typename T, 
    typename AtomicTraits = DefaultAtomicTraits<T>
>
class SCLAUDIO_API Atomic
{
public:   
    typedef Atomic<T, AtomicTraits> this_type;
    typedef T                       value_type;
            
    /**
        Constructs an atomic value using the nullary constructor `T()`.
        \note
            Non-atomic operation.
     */
    Atomic() 
        : mAddress( new T ) {}
    
    /**
        Constructs an atomic value using a unary constructor `T(U x)`.
        \note
            Non-atomic operation.
     */
    template <
        class U
    > 
    explicit Atomic(U value) 
        : mAddress( new T(value) ) {}

    /**
        Copy constructor.
        Creates a new atomic value by copying the value of the given atomic variable.
        \note
            Non-atomic operation.
     */
    Atomic(const Atomic<T>& other)
        : mAddress( new T(other.value()) ) {}

    /**
        Destroys the atomic value.
        \note
            Non-atomic operation.
     */
    virtual ~Atomic()
    {
        delete (T*) mAddress;
    }   

    /**
        Assignment operator.
        Replaces the current value with the value of the given atomic variable.
        \note
            Atomic operation.
     */
    void operator =(const Atomic<T>& other)
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
        return AtomicTraits::get((T*) mAddress);
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
            oldVal = AtomicTraits::get((T*) mAddress); 
        while (!tryReplace(oldVal, newVal));
    }
    
    /**
        Replace the current value with the given new value, if the given old value 
        equals the current value.
        \note
            Atomic operation.
     */
    inline bool tryReplace(T oldVal, T newVal)
    {
        return AtomicTraits::swap((T*) mAddress, oldVal, newVal);
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
        while(!tryModify(function));
    }

    /**
        Attempt to update the current value by applying the given function.

        If the value was modified, this method returns `true`.
        
        If the current value changed while the new value was being computed, no modification
        occurs and this method returns `false`.
        
        \note
            Atomic operation.
     */
    inline bool tryModify(Function<T,T> function)
    {
        T oldVal, newVal;
        oldVal = AtomicTraits::get((T*) mAddress);
        newVal = function(oldVal);
        return AtomicTraits::swap((T*) mAddress, oldVal, newVal);
    }
                  
protected:
    void*  mAddress;
};
              

/**
    An atomic number.
    
    This class extends Atomic with the usual increment and decrement operators. There are no ordinary arithmetic
    operators, as this class represents a variable and not a value.
 */
template <
    typename T, 
    typename AtomicTraits = DefaultAtomicTraits<T>
>
class SCLAUDIO_API AtomicNumber : public Atomic<T, AtomicTraits>
{
public:
    typedef AtomicNumber<T, AtomicTraits> this_type;
    typedef Atomic<T, AtomicTraits>       parent_type;

    /**
        Constructs an atomic number using a nullary constructor.
        \note
            Non-atomic operation.
     */
    AtomicNumber() 
        : Atomic<T>() {}
    
    /**
        Constructs an atomic number using a unary constructor.
        \note
            Non-atomic operation.
     */
    template <
        class U
    > 
    explicit AtomicNumber(U value) 
        : Atomic<U>(value) {}
    
    /**
        Destroys the atomic number.
        \note
            Non-atomic operation.
     */
    ~AtomicNumber() {}    

    /**
        Assignment operator.
        Replaces the current value with the value of the given atomic number.
        \note
            Atomic operation.
     */
    void operator =(const AtomicNumber<T>& other)
    {
        replace(other.value());
    }

    /**
        Prefix increment operator.
        \note
            Atomic operation.
     */
    inline Atomic<T, AtomicTraits>& operator ++()
    {
        increment(1);
        return *this;
    }

    /**
        Prefix decrement operator.
        \note
            Atomic operation.
     */
    inline Atomic<T, AtomicTraits>& operator --()
    {
        decrement(1);
        return *this;
    }

    /**
        Postfix increment operator.
        \note
            Atomic operation.
     */
    inline Atomic<T, AtomicTraits> operator ++(int post)
    {              
        AtomicNumber temp (this);
        increment(1);
        return temp;
    }

    /**
        Postfix decrement operator.
        \note
            Atomic operation.
     */
    inline Atomic<T, AtomicTraits> operator --(int post)
    {
        AtomicNumber temp (this);
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
        return AtomicTraits::add((T*) Atomic<T, AtomicTraits>::mAddress, amount);
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
};

template <class T>
bool operator ==(const Atomic<T>& a, const Atomic<T>& b)
{
    return a.value() == b.value();
}  

template <class T>
bool operator !=(const Atomic<T>& a, const Atomic<T>& b)
{
    return a.value() != b.value();	    
}

template <class T>
bool operator ==(const AtomicNumber<T>& a, const AtomicNumber<T>& b)
{
    return a.value() == b.value();
}  

template <class T>
bool operator !=(const AtomicNumber<T>& a, const AtomicNumber<T>& b)
{
    return a.value() != b.value();	    
}
   

typedef AtomicNumber<char>              atomic_char;
typedef AtomicNumber<unsigned char>     atomic_uchar;
typedef AtomicNumber<signed char>       atomic_schar;
typedef AtomicNumber<unsigned short>    atomic_ushort;
typedef AtomicNumber<short>             atomic_short;
typedef AtomicNumber<unsigned int>      atomic_uint;
typedef AtomicNumber<int>               atomic_int;
typedef AtomicNumber<unsigned long>     atomic_ulong;
typedef AtomicNumber<long>              atomic_long;
typedef AtomicNumber<uint8_t>           atomic_uint8_t;
typedef AtomicNumber<int8_t>            atomic_int8_t;
typedef AtomicNumber<uint16_t>          atomic_uint16_t;
typedef AtomicNumber<int16_t>           atomic_int16_t;
typedef AtomicNumber<uint32_t>          atomic_uint32_t;
typedef AtomicNumber<int32_t>           atomic_int32_t;
typedef AtomicNumber<uint64_t>          atomic_uint64_t;
typedef AtomicNumber<int64_t>           atomic_int64_t;
typedef AtomicNumber<void*>             atomic_address;
typedef AtomicNumber<bool>              atomic_bool;


} // namespace
} // namespace

#endif
