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
class SCLAUDIO_API SingleIterator 
    : public std::iterator<std::input_iterator_tag, T>
{ 
public:
    SingleIterator(T* x) 
        : mPtr(x) {}
    SingleIterator(const SingleIterator& mit) 
        : mPtr(mit.mPtr) {}
    
    SingleIterator& operator++() { ++mPtr; return *this; }
    SingleIterator operator++(T) { SingleIterator tmp(*this); operator++(); return tmp; }
    bool operator==(const SingleIterator& other) { return mPtr == other.mPtr; }
    bool operator!=(const SingleIterator& other) { return mPtr != other.mPtr; }
    T& operator*() { return *mPtr; }
private:
    T* mPtr;
};

/**
    A trivial singleton collection.
 */
template <class T> class SCLAUDIO_API Single
{
public:                                     
    typedef T                 value_type;
    typedef SingleIterator<T> iterator;
    typedef SingleIterator<T> const_iterator;
    typedef T&                reference_type;
    typedef const T&          const_reference;
    typedef T*                pointer_type;
    typedef ptrdiff_t         difference_type;
    typedef size_t            size_type;
    
    Single(T value)
        : mValue(value) {}

    iterator begin()
    {
        return iterator(&mValue);
    }
    iterator end()
    {
        return ++iterator(&mValue);
    }

    virtual ~Single() {}
private:
    T mValue;
};


} // namespace
} // namespace

#endif
