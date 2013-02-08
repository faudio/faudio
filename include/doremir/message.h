
#ifndef _DOREMIR_MESSAGE
#define _DOREMIR_MESSAGE

#include <doremir.h>
#include <doremir/std.h>
#include <doremir/pair.h>
#include <doremir/list.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirMessage Message
    @{
    */

typedef doremir_ptr_t doremir_message_address_t;
typedef doremir_ptr_t doremir_message_t;
typedef struct {
            void (* sync)(doremir_ptr_t);
            doremir_list_t (* receive)(doremir_ptr_t,
                                       doremir_message_address_t);
        } doremir_message_sender_t;
typedef struct {
            void (* send)(doremir_ptr_t,
                          doremir_message_address_t,
                          doremir_message_t);
        } doremir_message_receiver_t;
typedef struct _doremir_message_dispatcher_t * doremir_message_dispatcher_t;
doremir_message_dispatcher_t doremir_message_create_dispatcher();
doremir_message_dispatcher_t doremir_message_create_lockfree_dispatcher();
void doremir_message_destroy_dispatcher(doremir_message_dispatcher_t);
void doremir_message_send(doremir_ptr_t,
                          doremir_message_address_t,
                          doremir_message_t);
void doremir_message_sync(doremir_ptr_t);
doremir_list_t doremir_message_receive(doremir_ptr_t,
                                       doremir_message_address_t);

/** @}
    @}
    */

#endif // _DOREMIR_MESSAGE

