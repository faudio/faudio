
#ifndef _DOREMIR_AUDIO_DISPATCHER
#define _DOREMIR_AUDIO_DISPATCHER

#include <doremir/std.h>
#include <doremir/list.h>

/** @defgroup Doremir
    @{
    @defgroup Audio
    @{
    @defgroup Dispatcher
    @{
    */

typedef doremir_list_t doremir_audio_dispatcher_message_t;
typedef struct {
            void (* receive)(doremir_list_t);
            void (* send)(void (*)(doremir_list_t));
        } doremir_dispatcher_t;

/** @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIO_DISPATCHER

