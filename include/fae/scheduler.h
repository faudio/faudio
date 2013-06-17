
#ifndef _FAE_SCHEDULER
#define _FAE_SCHEDULER

#include <fae.h>
#include <fae/time.h>
#include <fae/event.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeScheduler Scheduler
    @{
    */

typedef struct _fae_scheduler_t * fae_scheduler_t;
fae_scheduler_t fae_scheduler_create(fae_time_clock_t);
void fae_scheduler_destroy(fae_scheduler_t);
void fae_scheduler_schedule(fae_scheduler_t, fae_event_t);
void fae_scheduler_execute(fae_scheduler_t);
void fae_scheduler_loop(fae_scheduler_t);
void fae_scheduler_loop_for(fae_time_t, fae_scheduler_t);

/** @}
    @}
    */

#endif // _FAE_SCHEDULER

