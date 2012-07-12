/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_FUTURE
#define _SCLAUDIOX_UTIL_FUTURE

#include "sclaudiox/core.h"

namespace doremir {
namespace scl {

class no_value {};

template <class T>
class future
{
public:
    /** Prevent the computation from occuring. */
    void interrupt();

    /** Temporarily prevent the computation from occuring. */
    void disable();

    /** Enable the computation from occuring after a previous call to disable(). */
    void enable();

    /** Block until the computation occurs. This may block forever. */
    void wait();

    /** Block until the computation occurs or the given amount of time has passed. */
    void wait_timeout(Time t);

    /** Whether or not the computation has occured. */
    bool is_done();
    
    /** Access the value, if available. */
    maybe<T> value() throw no_value;
};



} // namespace doremir
} // namespace scl

#endif
