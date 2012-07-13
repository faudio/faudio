/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

/**
    \file       util/single.h
    \brief      Trivial collection.
  */

#ifndef _SCLAUDIOX_UTIL_SINGLE
#define _SCLAUDIOX_UTIL_SINGLE

#include <iterator>

#include "sclaudiox/core.h"
#include "sclaudiox/error.h"
#include "sclaudiox/defines.h"

namespace doremir {
namespace scl {

template <
    class T
>
class SCLAUDIO_API pointer_iterator 
    : public std::iterator<std::input_iterator_tag, T>
{ 
public:
    pointer_iterator(T* x) 
        : mPtr(x) {}
    pointer_iterator(const pointer_iterator& mit) 
        : mPtr(mit.mPtr) {}
    
    pointer_iterator& operator++() { ++mPtr; return *this; }
    pointer_iterator operator++(T) { pointer_iterator tmp(*this); operator++(); return tmp; }
    bool operator==(const pointer_iterator& other) { return mPtr == other.mPtr; }
    bool operator!=(const pointer_iterator& other) { return mPtr != other.mPtr; }
    T& operator*() { return *mPtr; }
private:
    T* mPtr;
};

/**
    A trivial singleton collection.
 */
template <class T> class SCLAUDIO_API pointer
{
public:                                     
    typedef T                 value_type;
    typedef pointer_iterator<T> iterator;
    typedef pointer_iterator<T> const_iterator;
    typedef T&                reference_type;
    typedef const T&          const_reference;
    typedef T*                pointer_type;
    typedef ptrdiff_t         difference_type;
    typedef size_t            size_type;
    
    pointer(T value)
        : mValue(value) {}

    iterator begin()
    {
        return iterator(&mValue);
    }
    iterator end()
    {
        return ++iterator(&mValue);
    }

    virtual ~pointer() {}
private:
    T mValue;
};


} // namespace
} // namespace

#endif
