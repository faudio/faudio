/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_REALTIME_SCHEDULER
#define _SCLAUDIOX_SCHEDULING_REALTIME_SCHEDULER

#include "sclaudiox/scheduling/scheduler.h"
#include "sclaudiox/scheduling/future.h"
#include "sclaudiox/util/logging.h"

#ifdef SCL_LOG
    #ifndef SCL_NO_SCHEDULER_LOG
        #define SCL_LOG_SCHEDULER 1000
    #endif
#endif

namespace doremir {
namespace scl {

/**
    Base implementation of a realtime scheduler.
   
    Calls to schedule() and executePending() are guarded by a mutex, so any thread may
    safely call any of these methods. Currently calls to schedule are not reentrant, so if
    an action called out by an executing thread calls back to the schedule function the thread
    will deadlock.
 */
class SCLAUDIO_API RealtimeScheduler : public NonCopyable, public Scheduler
{
public:           
    RealtimeScheduler(TimeProvider* timer) 
        : timeSource(timer)
        , latency(0) {}

    Future* schedule(Future* future)
    {
        Lock lock (queueMutex);
        queue.push(acquire(future));        
        return future;
    }

    void execute(Time time)
    {
        Lock lock (queueMutex);

#ifdef SCL_LOG_SCHEDULER
            SCL_WRITE_LOG_IFN( time % SCL_LOG_SCHEDULER,
                               "Scheduler: "    << this 
                            << "  Thread : "    << boost::this_thread::get_id() 
                            << "  Queue size: " << queue.size() 
                            << "  Top: "        << (queue.size() ? queue.top()->time() : -1) 
                            << "  Time: "       << time << "\n" );
#endif

        while (queue.size()    > 0 
                   &&  
               (time - latency) >= queue.top()->time()) 
        {
            Future* future = queue.top();
            queue.pop();

            future->execute(time);
            
            if (future->reshedule() && (time - latency) < future->time())
            {
                queue.push(future);
            }
            else 
            {
                release(future);
            }
        }
    }          
    
    void executePending()
    {
        execute(timeSource->millisecondTime());
    }


protected:    
    TimeProvider* timeSource;
    Time          latency;

private:    
    typedef std::vector<Future*> FutureQueueBase;

    typedef std::priority_queue< Future*, 
                                 FutureQueueBase, 
                                 PointerNotLess<Future*> > FutureQueue;

    FutureQueue   queue;
    Mutex         queueMutex;
};

} // namespace
} // namespace

#endif
