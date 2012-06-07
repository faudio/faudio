/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_QUEUE_ATOMIC_SINGLE
#define _SCLAUDIOX_UTIL_QUEUE_ATOMIC_SINGLE

#include "sclaudiox/core.h"
#include "sclaudiox/error.h"
#include "sclaudiox/defines.h"

#include "sclaudiox/util/atomic.h"
#include "sclaudiox/util/queue.h"

namespace doremir {
namespace scl {

/**
    Atomic single read, single write queue.

    This implementation dynamically allocates space for new elements, so the insert method never fails.

    \invariant
        T is default-constructable
    \invariant
        T is copyable and assignable
 */
template <class T> class SCLAUDIO_API LinkedAtomicQueue : public Queue<T>
{
public:   
    /**
        Constructor, not thread-safe.
     */
    LinkedAtomicQueue()
    {
        mFirst.replace(new Node());
        mDivide.replace(mFirst.value());
        mLast.replace(mFirst.value());
    }
        
    /**
        Destructor, not thread-safe.
     */
    virtual ~LinkedAtomicQueue() 
    {
        while(mFirst != mLast)
        {
            Node* temp  = mFirst.value();
            mFirst.replace(mFirst.value()->next.value());
            delete temp;
        }
        delete mLast.value();
    }
    
    bool insert(T newElem)
    {   
        while (mFirst != mDivide)
        {
            Node* temp = mFirst.value();
            mFirst.tryReplace(temp, temp->next.value());
            delete temp;
        }

        mLast.value()->elem = newElem;
        mLast.value()->next.replace(new Node());        
        
        mLast.tryReplace(mLast.value(), mLast.value()->next.value());
        return true;
    }

    bool remove(T* nextElem)
    {
        if (mDivide == mLast) 
            return false;
        else
        {
            *nextElem = mDivide.value()->elem;                   
            mDivide.tryReplace(mDivide.value(), mDivide.value()->next.value());
            return true;
        }
    }
    
private:
    struct Node
    {
        T             elem;
        Atomic<Node*> next;
    };

    Atomic<Node*> mFirst;
    Atomic<Node*> mDivide;
    Atomic<Node*> mLast;
};        

} // namespace
} // namespace

#endif
