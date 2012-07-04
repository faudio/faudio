/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_FIB_HEAP
#define _SCLAUDIOX_UTIL_FIB_HEAP

#include "sclaudiox/core.h"

namespace doremir {
namespace scl {



template <class T>
class fib_heap_iterator
{
public:
    fib_heap_iterator() {}
    ~fib_heap_iterator() {}
    // FIXME
private:
};



// Sequence
template <class T, class Alloc = std::allocator<T> >
class fib_heap
{
public:
    typedef T                       value_type;
    typedef fib_heap_iterator<T> iterator;
    typedef fib_heap_iterator<T> const_iterator;
    typedef T&                      reference_type;
    typedef const T&                const_reference;
    typedef T*                      pointer;
    typedef int                     difference_type;
    typedef int                     size_type;
    
    fib_heap() {}
    fib_heap(const fib_heap<T>& x) {}
    ~fib_heap() {}

    void operator=(const fib_heap<T>& x) {}

    const_iterator begin()
    {
        return iterator();
    }
    const_iterator end()
    {
        return ++iterator();
    }
    size_type size()
    {
        // FIXME
    }
    size_type max_size()
    {
        return size();
    }
    size_type empty()
    {
        return size == 0;
    }
    void swap(const fib_heap<T>& y)
    {
        swap(*this, y);
    }

private:
};

template <class T, class Alloc>
bool operator==(const fib_heap<T, Alloc>& x, const fib_heap<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator!=(const fib_heap<T, Alloc>& x, const fib_heap<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator<(const fib_heap<T, Alloc>& x, const fib_heap<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator>(const fib_heap<T, Alloc>& x, const fib_heap<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator<=(const fib_heap<T, Alloc>& x, const fib_heap<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
bool operator>=(const fib_heap<T, Alloc>& x, const fib_heap<T, Alloc>& y)
{
    // FIXME
}

template <class T, class Alloc>
void swap(const fib_heap<T, Alloc>& x, const fib_heap<T, Alloc>& y)
{
    // FIXME
}


} // namespace doremir
} // namespace scl

#endif
