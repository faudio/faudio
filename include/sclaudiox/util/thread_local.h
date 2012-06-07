/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_THREAD_LOCAL
#define _SCLAUDIOX_UTIL_THREAD_LOCAL

#include "sclaudiox/defines.h"
#include "sclaudiox/util/concurrency.h"

namespace doremir {
namespace scl {

// /**
//     A thread local variable.
//     
//     Invariant:
//       * There is a default constructor T().
//         To use for types without default constructors, use ThreadLocal< Maybe<T> > instead.
//  */
// template <class T> class SCLAUDIO_API ThreadLocal
// {
// public:
// 
//     /**
//         Constructor.
//      */
//     ThreadLocal()
//         : impl()
//         , init(NULL) {}
// 
//     /**
//         Destructor.
//      */
//     ~ThreadLocal()
//     {
//         if (init)
//             delete init;
//     }
// 
//     /**
//         Returns the thread local value of the pointer.
//      */
//     T* operator->()
//     {
//         return getValue();
//     }
// 
//     /**
//         Returns the thread local value of the pointer.
//      */
//     T& operator*()
//     {
//         return *getValue();
//     }
// 
// private:
//     T* getValue()
//     {
//         if (!impl.getValue() && !init) 
//             impl.reset(new T);
//         return impl.getValue();
//     }
// 
//     typedef boost::thread_specific_ptr<T> ImplT;
//     
//     T*    init;
//     ImplT impl;
// 
//     SCL_DISABLE_COPY_AND_ASSIGN(ThreadLocal);
// };                                      

} // namespace
} // namespace

#endif
