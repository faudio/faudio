/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_FUTURE_GROUP
#define _SCLAUDIOX_SCHEDULING_FUTURE_GROUP

#include "sclaudiox/scheduling/future.h"
#include "sclaudiox/scheduling/interruptible.h"

namespace doremir {
namespace scl {

enum InterruptionMode
{
    kSimple,
    kForcing,
    kTransactional
};

/**
    Provides an atomic interrupt for multiple events.
 */
class SCLAUDIO_API FutureGroup 
    : public NonCopyable
    , public Resource
    , public Interruptible
{
public:
    explicit FutureGroup(InterruptionMode mode) 
        : interruptionMode(mode)
        , interrupted(false)
        , count(0) {}

    ~FutureGroup()
    {
        // TODO
    }

    void interrupt();

    InterruptionMode const interruptionMode;

private:
    friend class Future;
    void add(Future* future);
    void remove(Future* future);

    std::list<Future*>  members;
    bool                interrupted;
    int                 count;
    Mutex               mutex;
};

} // namespace
} // namespace

#endif
