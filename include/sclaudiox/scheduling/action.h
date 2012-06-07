/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_SCHEDULING_ACTION
#define _SCLAUDIOX_SCHEDULING_ACTION


namespace doremir {
namespace scl {

/** 
    An action to be invoked at a specific time. 
 */
class SCLAUDIO_API Action
{
public: 
    virtual ~Action() {}
    virtual void accept(Time time) const = 0;
};


/**
    Used to schedule actions, which are callback functions with accurate time 
    information.
 */
class SCLAUDIO_API ActionScheduler : public Scheduler
{
public:
    virtual Future* scheduleNow(Action* action, 
                                ScheduleOptions options = ScheduleOptions()) = 0;
    
    virtual Future* scheduleLater(Action* action, 
                                  Time time, 
                                  ScheduleOptions options = ScheduleOptions()) = 0;
    
    virtual Future* scheduleAt(Action* action, 
                               Time time, 
                               ScheduleOptions options = ScheduleOptions()) = 0;
};

/**
    A future that invokes an action.
 */
class SCLAUDIO_API ActionFuture : public Future
{
public:
    ActionFuture(Action* action, 
                 Time time, 
                 ScheduleOptions options)
        : Future(time, options)
        , action(action) {}

    inline void perform(Time now)
    {
        action->accept(now);
    }
    
protected:
    Action* action;
};
          
} // namespace
} // namespace

#endif
