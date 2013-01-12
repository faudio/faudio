
#ifndef _DOREMIR_MESSAGE
#define _DOREMIR_MESSAGE

#include <doremir.h>
#include <doremir/std.h>
#include <doremir/pair.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirMessage Message
    @{
    */

typedef doremir_ptr_t doremir_message_address_t;
typedef doremir_ptr_t doremir_message_t;
typedef struct {
            void (* receive)(doremir_ptr_t,
                             doremir_message_address_t,
                             doremir_message_t);
        } doremir_message_receiver_t;
typedef struct {
            void (* add_receiver)(doremir_ptr_t, doremir_message_receiver_t);
            void (* remove_receiver)(doremir_ptr_t,
                                     doremir_message_receiver_t);
            void (* dispatch)(doremir_ptr_t);
        } doremir_message_sender_t;
typedef struct _doremir_message_dispatcher_t * doremir_message_dispatcher_t;
doremir_message_dispatcher_t doremir_message_simple();
void doremir_message_destroy(doremir_message_dispatcher_t);
doremir_pair_t doremir_message_buffered();
doremir_pair_t doremir_message_non_blocking();

/** @}
    @}
    */

#endif // _DOREMIR_MESSAGE

