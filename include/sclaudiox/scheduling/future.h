/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_SCHEDULING_FUTURE
#define  _SCLAUDIOX_SCHEDULING_FUTURE

#include <queue>

#include "sclaudiox/core.h"
#include "sclaudiox/control.h"

#include "sclaudiox/util/concurrency.h"
#include "sclaudiox/util/foreach.h"

#include "sclaudiox/device/audio.h"
#include "sclaudiox/device/midi.h"

#include "sclaudiox/scheduling/scheduler.h"
#include "sclaudiox/scheduling/future_group.h"

namespace doremir {
namespace scl {

enum FutureType 
{
    kMessageFuture,
    kActionFuture,
    kSendingFuture,
    kReceivingFuture
};


/**
    A future is an handle to an asynchronous computation, such as an Action scheduledto be
    triggered or a Message scheduled to be delivered at some point in time. 
 */
class SCLAUDIO_API Future : public Resource, public Interruptible
{
public:
    static const Time forcedExecutionTime = -1;

    Future(Time time, 
           ScheduleOptions options)
        : nextTime(time)
        , interrupted(0)
        , count(0)
    {
        groups   = options.groups;
        repeats  = options.repeats;
        interval = options.interval;
        
        foreach(FutureGroup* group, groups)
        {
            group->add(acquire(this));
        }
    }          

    virtual ~Future() {}

    /**
        Prevents this future from being executed the next time 
        it is dequeued.
     */
    void interrupt();

    /**
        Executes this future iff interrupt has not been called and
        an interrupt has not been issued to a group of which this
        future is a member.
     */
    void execute(Time now);
    
    /**
        Force an immedate execution and interruption of this
        future.
     */
    void forcedExecute();
 

    /**
        Informs the scheduler whether this future should be resheduled
        of not.
     */
    bool reshedule();

    /**
        Ideal time of next execution.
        Should only be called from the enclosing Scheduler.
     */
    inline Time time()
    {
        return nextTime;
    }

    /**
        Compare on time.
        Should only be called from the enclosing Scheduler.
     */
    inline virtual bool operator<(const Future& other) 
    { 
        return nextTime < other.nextTime; 
    }
                   
protected:    
    /**
        Called by execute() to carry out the action.
     */
    virtual void perform(Time now) = 0;


private:
    friend class Scheduler;
    friend class FutureGroup;
    void operator=(const Future&);

    Time                    nextTime;
    std::list<FutureGroup*> groups;
    int                     repeats; 
    Time                    interval;
    
    // Mutable
    bool                    interrupted;
    int                     count;
    Mutex                   mutex;
};


// =============================================================================

class SCLAUDIO_API NilFuture : public Future
{
public:
    NilFuture(Time time, ScheduleOptions options) 
        : Future(time, options) {}

    inline void perform(Time now) 
    {
        // nothing
    }
};


} // namespace
} // namespace

#endif
