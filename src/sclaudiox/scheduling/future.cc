/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
/**
    @file   sclaudiox/scheduling/future.cc
    @author Hans Hoglund
 */

#include "sclaudiox/scheduling/future.h"
#include "sclaudiox/util/foreach.h"

namespace doremir {
namespace scl {

void FutureGroup::remove(Future* future)
{
    std::remove(members.begin(), members.end(), future);
}

void FutureGroup::add(Future* future)
{
    members.push_back(future);
}

void FutureGroup::interrupt()
{
    Lock privateLock (mutex);
    switch (interruptionMode)
    {
        case kSimple:
            interrupted = true;
            break;

        case kTransactional:
            if (count == 0)
                interrupted = true;
            break;

        case kForcing:
            {
                foreach( Future* member, members )
                    member->forcedExecute();
            }
            interrupted = true;
            break;

        default: 
            throw Impossible();
    }
}

// =============================================================================

void Future::interrupt()
{
    Lock privateLock (mutex);
    interrupted = true;
}

void Future::execute(Time now)
{
    Lock privateLock (mutex);

    if (interrupted)
        return;

    foreach(FutureGroup* group, groups)
    {
        Lock groupLock (group->mutex);
        
        if (group->interrupted)
        {
            interrupted = true; 
            return;
        }
        else
            group->count++;
    }
    perform(now);
    
    nextTime = now + interval;
    count++;
}

void Future::forcedExecute()
{
    Lock privateLock (mutex);
    
    if (interrupted || (count >= repeats && repeats >= 0)) 
        return;
        
    interrupted = true;
    perform(Future::forcedExecutionTime);
}

bool Future::reshedule()
{
    return !interrupted && (count < repeats || repeats < 0);
}

} // namespace
} // namespace
