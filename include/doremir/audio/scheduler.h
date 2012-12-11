
#ifndef _DOREMIR_AUDIO_SCHEDULER
#define _DOREMIR_AUDIO_SCHEDULER

#include <doremir/thread/improving.h>

/** @defgroup Doremir
    @{
    @defgroup Audio
    @{
    @defgroup Scheduler
    @{
    */

typedef int doremir_audio_scheduler_time_t;
typedef void (* doremir_audio_scheduler_action_t)();
typedef struct {
            void (* schedule)(doremir_audio_scheduler_time_t,
                              doremir_audio_scheduler_action_t);
            void (* execute)();
        } doremir_scheduler_t;
doremir_scheduler_t doremir_audio_scheduler_create(doremir_improving_t);
void doremir_audio_scheduler_destroy(doremir_scheduler_t);
void doremir_audio_scheduler_swap(doremir_scheduler_t,
                                  doremir_scheduler_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIO_SCHEDULER

