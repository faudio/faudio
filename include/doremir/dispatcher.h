
#ifndef _DOREMIR_DISPATCHER
#define _DOREMIR_DISPATCHER

#include <doremir/std.h>
#include <doremir/list.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDispatcher Dispatcher
    @{
    */

typedef doremir_list_t doremir_dispatcher_message_t;
typedef struct {
            void (* receive)(doremir_list_t);
            void (* send)(void (*)(doremir_list_t));
        } doremir_dispatcher_t;

/** @}
    @}
    */

#endif // _DOREMIR_DISPATCHER

