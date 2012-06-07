/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_REALTIME_ACTION
#define _SCLAUDIOX_SCHEDULING_REALTIME_ACTION

#include "sclaudiox/scheduling/realtime/scheduler.h"

namespace doremir {
namespace scl {

class SCLAUDIO_API RealtimeActionScheduler : public ActionScheduler, 
                                             public RealtimeScheduler
{
public:
    Future* scheduleNow(Action* action, 
                        ScheduleOptions options)
    {
        Time now = timeSource->millisecondTime();
        return schedule(new ActionFuture(action, now, options));
    }

    Future* scheduleLater(Action* action, 
                          Time time, 
                          ScheduleOptions options)
    {
        Time now = timeSource->millisecondTime();
        return schedule(new ActionFuture(action, now + time, options));
    }

    Future* scheduleAt(Action* action, 
                       Time time, 
                       ScheduleOptions options)
    {
        Time now = timeSource->millisecondTime();
        Future* future = new ActionFuture(action, time, options);
        
        if (now - latency <= time) 
            schedule(future);
        return future;
    }
    
    RealtimeActionScheduler(TimeProvider* timeSource) 
        : RealtimeScheduler(timeSource) {}

protected:
    Future* schedule(Future* future)
    {
        return RealtimeScheduler::schedule(future);
    }
    void execute(Time time)
    {
        return RealtimeScheduler::execute(time);
    }

};

} // namespace
} // namespace

#endif
