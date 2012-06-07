/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_SEMAPHORE
#define _SCLAUDIOX_UTIL_SEMAPHORE

#include "sclaudiox/core.h"
#include "sclaudiox/defines.h"
#include "sclaudiox/util/atomic.h"
#include "sclaudiox/util/concurrency.h"

namespace doremir {
namespace scl {

// /**
//     Nonblocking semaphore interface.
//     
//     Invariants:
//         Atomic => T
//  */
// template <class T> class Semaphore
// {
// public:
//  Semaphore()
//      : none()
//      , current() {}
// 
//     virtual ~Semaphore() {}
//     
//     /**
//         Attempt to acquire this semaphore, returning true upon success, false upon failure.
//      */
//     virtual bool acquire(T id)
//     {
//         return atomicCompareAndSwap(&current, none, id);
//     }
// 
//  virtual bool alwaysAcquire(T id)
//     {
//      // FIXME should use fetchAndStore
//      current = id;
//      return true;
//     }
// 
//     /**
//         Release this semaphore, if holding it.
//      */
//     virtual void release(T id)
//     {
//         atomicCompareAndSwap(&current, id, none);
//     }
// 
//  bool holds(T id)
//     {
//         return current == id;
//     }
// 
//  bool isFree()
//     {
//         return current == none;
//     }
// 
// private:
//  T none;
//     T current;    
//     
//  SCL_DISABLE_COPY_AND_ASSIGN(Semaphore);
// };
// 
// /**
//     Nonblocking semaphore that may be intercepted.
//  */
// template <class T> class ContestedSemaphore : public Semaphore<T>
// {
// public:
//     typedef Semaphore<T> Parent;
// 
//     virtual bool acquire(T id, void (*released)(void*), void* data)
//     {
//         if (Parent::alwaysAcquire(id))
//         {
//             Lock lock (mutex);
//             if (releaseFunction)
//                 releaseFunction(releaseFunctionData);
//             releaseFunction      = released;
//          releaseFunctionData = data;
//             return true;
//         }      
//         return false;
//     }
// 
//     bool acquire(T id)
//     {
//         return acquire(id, NULL, NULL);
//     }  
// 
//     void release(T id)
//     {
//      Parent::release(id);
//     }
// 
//  ContestedSemaphore()
//      : releaseFunction(NULL)
//      , releaseFunctionData(NULL) {};
// 
// private:
//  void (*releaseFunction)(void*);
//  void* releaseFunctionData;
//     Mutex mutex;
// };
// 
// 
// 
// template <class T> class PredicateSemaphore : public ContestedSemaphore<T>
// {
// public:
//     typedef ContestedSemaphore<T> Parent;
// 
//     virtual bool predicate(T id) = 0;  
//     
//     bool acquire(T id, void (*released)(void*), void* data)
//     {       
//         Lock lock (predMutex);
//         if (predicate(id))
//             return Parent::acquire(id, released, data);
//      return false;
//     }
// 
//     bool acquire(T id)
//     {
//         return acquire(id, NULL, NULL);
//     }  
// 
//     void release(T id)
//     {
//         Parent::release(id);
//     }     
// private:
//  Mutex predMutex;
// };
// 
// 
// template <class T> class HoldsSemaphore : public PredicateSemaphore<T>
// {
// public:       
//     bool predicate(T id)
//     {
//         return semaphore->holds(id);
//     }
//     HoldsSemaphore(Semaphore<T>* semaphore)
//         : semaphore(semaphore) {}
// private:
//     Semaphore<T>* semaphore;
// };
// 
// template <class T> class DoesNotHoldSemaphore : public PredicateSemaphore<T>
// {
// public:     
//     bool predicate(T id)
//     {
//         return !semaphore->holds(id);
//     }     
//     DoesNotHoldSemaphore(Semaphore<T>* semaphore)
//         : semaphore(semaphore) {}
// private:
//     Semaphore<T>* semaphore;
// };
// 
// 
// template <class T> class SemaphoreIsFree : public PredicateSemaphore<T>
// {
// public:     
//     bool predicate(T id)
//     {
//      return semaphore->isFree();
//     }       
//     SemaphoreIsFree(Semaphore<T>* semaphore)
//         : semaphore(semaphore) {}
// 
// // FIXME see device/audio.h
// //private:
//  Semaphore<T>* semaphore;
// };
// 
// template <class T> class SemaphoreIsTaken : public PredicateSemaphore<T>
// {
// public:     
//     bool predicate(T id)
//     {
//      return !semaphore->isFree();
//     }  
//     SemaphoreIsTaken(Semaphore<T>* semaphore)
//         : semaphore(semaphore) {}
// private:
//     Semaphore<T>* semaphore;
// };
//       

} // namespace
} // namespace

#endif
