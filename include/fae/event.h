
#ifndef _FAE_EVENT
#define _FAE_EVENT

#include <fae.h>
#include <fae/std.h>
#include <fae/time.h>
#include <fae/message.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeEvent Event
    @{
    */

typedef struct _fae_event_t * fae_event_t;
fae_event_t fae_event_never();
fae_event_t fae_event_now(fae_ptr_t);
fae_event_t fae_event_delay(fae_time_t, fae_event_t);
fae_event_t fae_event_merge(fae_event_t, fae_event_t);
fae_event_t fae_event_switch(fae_event_t,
                             fae_event_t,
                             fae_event_t);
fae_event_t fae_event_receive(fae_message_sender_t,
                              fae_message_address_t);
fae_event_t fae_event_send(fae_message_receiver_t,
                           fae_message_address_t,
                           fae_event_t);
void fae_event_destroy(fae_event_t);
fae_event_t fae_event_first(fae_event_t);
fae_event_t fae_event_rest(fae_event_t);
fae_event_t fae_event_later(fae_time_t, fae_ptr_t);
fae_event_t fae_event_repeat(fae_time_t, fae_ptr_t);
fae_event_t fae_event_after(fae_event_t, fae_event_t);
fae_event_t fae_event_before(fae_event_t, fae_event_t);
fae_event_t fae_event_sample(fae_event_t, fae_event_t);
fae_event_t fae_event_toggle(fae_event_t, fae_event_t);
fae_event_t fae_event_filter(fae_pred_t, fae_ptr_t, fae_event_t);
fae_event_t fae_event_dfilter(fae_pred_t, fae_ptr_t, fae_event_t);
fae_event_t fae_event_map(fae_unary_t, fae_ptr_t, fae_event_t);
fae_event_t fae_event_dmap(fae_unary_t, fae_ptr_t, fae_event_t);
fae_event_t fae_event_map2(fae_binary_t,
                           fae_ptr_t,
                           fae_event_t,
                           fae_event_t);
fae_event_t fae_event_dmap2(fae_binary_t,
                            fae_ptr_t,
                            fae_event_t,
                            fae_event_t);
fae_time_t fae_event_offset(fae_event_t);
void fae_event_add_sync(void (*)(fae_ptr_t, fae_message_sender_t),
                        fae_ptr_t,
                        fae_event_t);
fae_list_t fae_event_values(fae_time_t, fae_time_t, fae_event_t);
bool fae_event_has_values(fae_time_t, fae_time_t, fae_event_t);
bool fae_event_has_more(fae_time_t, fae_event_t);

/** @}
    @}
    */

#endif // _FAE_EVENT

