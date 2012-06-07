/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_HANDLER
#define _SCLAUDIOX_SCHEDULING_HANDLER

namespace doremir {
namespace scl {

/** 
    Interface defining an action to take upon errors in the scheduling system. 
 */           
template <class T>
class Handler
{
public:       
    virtual ~Handler() {}
    virtual void accept(Time time, T& object) const = 0;
};

} // namespace
} // namespace

#endif
