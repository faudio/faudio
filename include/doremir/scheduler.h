
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

typedef doremir_nullary_t doremir_scheduler_action_t;
typedef struct _doremir_scheduler_t * doremir_scheduler_t;
doremir_scheduler_t doremir_scheduler_create(doremir_ptr_t);
void doremir_scheduler_destroy(doremir_scheduler_t);
void doremir_scheduler_schedule(doremir_event_t,
                                doremir_scheduler_t);
void doremir_scheduler_execute(doremir_scheduler_t);
void doremir_scheduler_run(doremir_scheduler_t);

/** @}
    @}
    */

#endif // _DOREMIR_SCHEDULER

