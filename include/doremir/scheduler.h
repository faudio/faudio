
#ifndef _DOREMIR_SCHEDULER
#define _DOREMIR_SCHEDULER

#include <doremir.h>
#include <doremir/time.h>
#include <doremir/event.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirScheduler Scheduler
    @{
    */

typedef struct _doremir_scheduler_t * doremir_scheduler_t;
doremir_scheduler_t doremir_scheduler_create(doremir_time_clock_t);
void doremir_scheduler_destroy(doremir_scheduler_t);
void doremir_scheduler_schedule(doremir_scheduler_t,
                                doremir_event_t);
void doremir_scheduler_execute(doremir_scheduler_t);
void doremir_scheduler_loop(doremir_scheduler_t);
void doremir_scheduler_loop_for(doremir_time_t,
                                doremir_scheduler_t);

/** @}
    @}
    */

#endif // _DOREMIR_SCHEDULER

