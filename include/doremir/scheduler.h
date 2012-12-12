
#ifndef _DOREMIR_SCHEDULER
#define _DOREMIR_SCHEDULER

#include <doremir/thread/improving.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirScheduler Scheduler
    @{
    */

typedef int doremir_scheduler_time_t;
typedef void (* doremir_scheduler_action_t)();
typedef struct {
            void (* schedule)(doremir_scheduler_time_t,
                              doremir_scheduler_action_t);
            void (* execute)();
        } doremir_scheduler_t;
doremir_scheduler_t doremir_scheduler_create(doremir_thread_improving_t);
void doremir_scheduler_destroy(doremir_scheduler_t);
void doremir_scheduler_swap(doremir_scheduler_t,
                            doremir_scheduler_t);

/** @}
    @}
    */

#endif // _DOREMIR_SCHEDULER

