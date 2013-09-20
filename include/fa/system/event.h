
#ifndef _FA_SYSTEM_EVENT
#define _FA_SYSTEM_EVENT

#include <fa.h>
#include <fa/message.h>
#include <fa/event.h>
#include <fa/string.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaSystem System
    @{
    @defgroup FaSystemEvent Event
    @{
    */

typedef enum {
            mouse_move_event,
            mouse_drag_event,
            mouse_up_event,
            mouse_down_event,
            key_up_event,
            key_down_event
        } fa_system_event_type_t;
fa_event_t fa_system_event_mouse_move();
fa_event_t fa_system_event_mouse_drag();
fa_event_t fa_system_event_mouse_up();
fa_event_t fa_system_event_mouse_down();
fa_event_t fa_system_event_key_up();
fa_event_t fa_system_event_key_down();
fa_event_t fa_system_event_select(fa_list_t);
fa_message_sender_t fa_system_event_receive(fa_list_t);
fa_event_t fa_system_event_write_std(fa_event_t);
fa_message_receiver_t fa_system_event_send_std();
fa_event_t fa_system_event_write_log(fa_event_t);
fa_message_receiver_t fa_system_event_send_log();

/** @}
    @}
    @}
    */

#endif // _FA_SYSTEM_EVENT

