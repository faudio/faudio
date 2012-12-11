
#ifndef _DOREMIR_AUDIOENGINE_DISPATCHER
#define _DOREMIR_AUDIOENGINE_DISPATCHER

#include <doremir/std.h>
#include <doremir/list.h>

/** @defgroup Doremir
    @{
    @defgroup AudioEngine
    @{
    @defgroup Dispatcher
    @{
    */

typedef doremir_list_t doremir_audio_engine_dispatcher_message_t;
typedef struct {
            void (* receive)(doremir_list_t);
            void (* send)(void (*)(doremir_list_t));
        } doremir_dispatcher_t;

/** @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_DISPATCHER

