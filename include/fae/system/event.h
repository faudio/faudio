
#ifndef _FAE_SYSTEM_EVENT
#define _FAE_SYSTEM_EVENT

#include <fae.h>
#include <fae/message.h>
#include <fae/event.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeSystem System
    @{
    @defgroup FaeSystemEvent Event
    @{
    */

typedef enum {
            mouse_move_event,
            mouse_drag_event,
            mouse_up_event,
            mouse_down_event,
            key_up_event,
            key_down_event
        } fae_system_event_type_t;
fae_event_t fae_system_event_mouse_move();
fae_event_t fae_system_event_mouse_drag();
fae_event_t fae_system_event_mouse_up();
fae_event_t fae_system_event_mouse_down();
fae_event_t fae_system_event_key_up();
fae_event_t fae_system_event_key_down();
fae_event_t fae_system_event_select(fae_list_t);
fae_message_sender_t fae_system_event_receive(fae_list_t);
fae_event_t fae_system_event_write_std(fae_event_t);
fae_message_receiver_t fae_system_event_send_std();
fae_event_t fae_system_event_write_log(fae_event_t);
fae_message_receiver_t fae_system_event_send_log();

/** @}
    @}
    @}
    */

#endif // _FAE_SYSTEM_EVENT

