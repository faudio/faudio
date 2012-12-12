
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
typedef intptr_t doremir_dispatcher_address_t;
typedef struct {
            void (* send)(doremir_dispatcher_address_t, doremir_list_t);
        } doremir_dispatcher_sender_t;
typedef struct {
            void (* receive)(doremir_dispatcher_sender_t *);
        } doremir_dispatcher_receiver_t;
typedef struct {
            void (* send)(doremir_dispatcher_address_t, doremir_list_t);
            void (* receive)(doremir_dispatcher_sender_t *);
        } doremir_dispatcher_t;

/** @}
    @}
    */

#endif // _DOREMIR_DISPATCHER

