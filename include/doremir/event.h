
#ifndef _DOREMIR_EVENT
#define _DOREMIR_EVENT

#include <doremir.h>
#include <doremir/std.h>
#include <doremir/time.h>
#include <doremir/message.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirEvent Event
    @{
    */

typedef struct _doremir_event_t * doremir_event_t;
doremir_event_t doremir_event_never();
doremir_event_t doremir_event_now(doremir_ptr_t);
doremir_event_t doremir_event_delay(doremir_time_t,
                                    doremir_event_t);
doremir_event_t doremir_event_merge(doremir_event_t,
                                    doremir_event_t);
doremir_event_t doremir_event_switch(doremir_event_t,
                                     doremir_event_t,
                                     doremir_event_t);
doremir_event_t doremir_event_receive(doremir_message_sender_t,
                                      doremir_message_address_t);
doremir_event_t doremir_event_send(doremir_message_receiver_t,
                                   doremir_message_address_t,
                                   doremir_event_t);
void doremir_event_destroy(doremir_event_t);
doremir_event_t doremir_event_first(doremir_event_t);
doremir_event_t doremir_event_rest(doremir_event_t);
doremir_event_t doremir_event_later(doremir_time_t, doremir_ptr_t);
doremir_event_t doremir_event_repeat(doremir_time_t,
                                     doremir_ptr_t);
doremir_event_t doremir_event_after(doremir_event_t,
                                    doremir_event_t);
doremir_event_t doremir_event_before(doremir_event_t,
                                     doremir_event_t);
doremir_event_t doremir_event_sample(doremir_event_t,
                                     doremir_event_t);
doremir_event_t doremir_event_toggle(doremir_event_t,
                                     doremir_event_t);
doremir_time_t doremir_event_offset(doremir_event_t);
void doremir_event_add_sync(void (*)(doremir_ptr_t,
                                     doremir_message_sender_t),
                            doremir_ptr_t,
                            doremir_event_t);
doremir_list_t doremir_event_values(doremir_time_t,
                                    doremir_time_t,
                                    doremir_event_t);
bool doremir_event_has_values(doremir_time_t,
                              doremir_time_t,
                              doremir_event_t);
bool doremir_event_has_more(doremir_time_t, doremir_event_t);

/** @}
    @}
    */

#endif // _DOREMIR_EVENT

