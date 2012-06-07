/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_INTERRUPTIBLE
#define _SCLAUDIOX_SCHEDULING_INTERRUPTIBLE

namespace doremir {
namespace scl {

/** 
    Interface defining the interrupt operation.
 */
class SCLAUDIO_API Interruptible
{
public:
    virtual ~Interruptible() {}
    virtual void interrupt() = 0;
};

} // namespace
} // namespace

#endif
