/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

/**
    \file       util/queue.h
    \brief      Basic queue interface.
  */

#ifndef _SCLAUDIOX_UTIL_QUEUE
#define _SCLAUDIOX_UTIL_QUEUE

#include "sclaudiox/core.h"
#include "sclaudiox/error.h"
#include "sclaudiox/defines.h"

namespace doremir {
namespace scl {

/**
    This interface defines a standard first-in, first-out queue.
 */
template <class T> class SCLAUDIO_API Queue : public NonCopyable
{
public:   
    Queue(){}
    virtual ~Queue() {}

    /**
        Add an element to the queue. Returns true if successful.
     */
    virtual bool insert(T elem) = 0;

    /**
        Remove an element from the queue. Returns true if successful.
     */
    virtual bool remove(T* elem) = 0;
};


} // namespace
} // namespace

#endif
