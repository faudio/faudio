
#ifndef _DOREMIR_AUDIOENGINE_SCHEDULER
#define _DOREMIR_AUDIOENGINE_SCHEDULER

#include <doremir/thread/improving.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAudioEngine AudioEngine
    @{
    @defgroup DoremirAudioEngineScheduler Scheduler
    @{
    */

typedef int doremir_audio_engine_scheduler_time_t;
typedef void (* doremir_audio_engine_scheduler_action_t)();
typedef struct {
            void (* schedule)(doremir_audio_engine_scheduler_time_t,
                              doremir_audio_engine_scheduler_action_t);
            void (* execute)();
        } doremir_audio_engine_scheduler_t;
doremir_audio_engine_scheduler_t doremir_audio_engine_scheduler_create(doremir_thread_improving_t);
void doremir_audio_engine_scheduler_destroy(doremir_audio_engine_scheduler_t);
void doremir_audio_engine_scheduler_swap(doremir_audio_engine_scheduler_t,
                                         doremir_audio_engine_scheduler_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_SCHEDULER

