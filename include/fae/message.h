
#ifndef _FAE_MESSAGE
#define _FAE_MESSAGE

#include <fae.h>
#include <fae/std.h>
#include <fae/pair.h>
#include <fae/list.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeMessage Message
    @{
    */

typedef fae_ptr_t fae_message_address_t;
typedef fae_ptr_t fae_message_t;
typedef struct {
            void (* sync)(fae_ptr_t);
            fae_list_t (* receive)(fae_ptr_t, fae_message_address_t);
        } fae_message_sender_interface_t;
typedef struct {
            void (* send)(fae_ptr_t, fae_message_address_t, fae_message_t);
        } fae_message_receiver_interface_t;
typedef struct _fae_message_sender_t * fae_message_sender_t;
typedef struct _fae_message_receiver_t * fae_message_receiver_t;
void fae_message_send(fae_message_receiver_t,
                      fae_message_address_t,
                      fae_message_t);
fae_list_t fae_message_receive(fae_message_sender_t,
                               fae_message_address_t);
void fae_message_sync(fae_message_sender_t);
typedef struct _fae_message_dispatcher_t * fae_message_dispatcher_t;
fae_message_dispatcher_t fae_message_create_dispatcher();
fae_message_dispatcher_t fae_message_create_lockfree_dispatcher();
void fae_message_destroy_dispatcher(fae_message_dispatcher_t);

/** @}
    @}
    */

#endif // _FAE_MESSAGE

