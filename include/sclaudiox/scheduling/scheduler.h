/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_SCHEDULER
#define _SCLAUDIOX_SCHEDULING_SCHEDULER

#include "sclaudiox/core.h"

namespace doremir {
namespace scl {

class Future;
class FutureGroup;

/**
    Options passed to an ActionScheduler when scheduling actions.
 */
struct ScheduleOptions : public Options
{
    TimeUnit                unit;
    std::list<FutureGroup*> groups; 
    int                     repeats; 
    Time                    interval;
    
    ScheduleOptions()
        : unit(kMilliseconds)
        , groups(std::list<FutureGroup*>(0))
        , repeats(1)
        , interval(0){}
};

/**
    Stores and executes future actions.
 */
class SCLAUDIO_API Scheduler
{
protected:
    virtual ~Scheduler() {}
    
    /**
        Schedule and return the given future object.
     */
    virtual Future* schedule(Future* future) = 0;

    /**
        Execute all events due at the given time.
     */
    virtual void execute(Time time) = 0;    
}; 

} // namespace
} // namespace

#endif
