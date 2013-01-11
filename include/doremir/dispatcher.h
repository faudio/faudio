
#ifndef _DOREMIR_DISPATCHER
#define _DOREMIR_DISPATCHER

#include <doremir.h>
#include <doremir/std.h>
#include <doremir/list.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDispatcher Dispatcher
    @{
    */

typedef doremir_ptr_t doremir_dispatcher_address_t;
typedef doremir_ptr_t doremir_dispatcher_message_t;
typedef struct {
            void (* receive)(doremir_ptr_t,
                             doremir_dispatcher_address_t,
                             doremir_dispatcher_message_t);
        } doremir_dispatcher_receiver_t;
typedef struct {
            void (* add_receiver)(doremir_ptr_t, doremir_ptr_t);
            void (* remove_receiver)(doremir_ptr_t, doremir_ptr_t);
        } doremir_dispatcher_t;

/** @}
    @}
    */

#endif // _DOREMIR_DISPATCHER

