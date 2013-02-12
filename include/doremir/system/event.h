
#ifndef _DOREMIR_SYSTEM_EVENT
#define _DOREMIR_SYSTEM_EVENT

#include <doremir.h>
#include <doremir/message.h>
#include <doremir/event.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirSystem System
    @{
    @defgroup DoremirSystemEvent Event
    @{
    */

typedef enum {
            mouse_move_event,
            mouse_drag_event,
            mouse_up_event,
            mouse_down_event,
            key_up_event,
            key_down_event
        } doremir_system_event_type_t;
doremir_event_t doremir_system_event_mouse_move();
doremir_event_t doremir_system_event_mouse_drag();
doremir_event_t doremir_system_event_mouse_up();
doremir_event_t doremir_system_event_mouse_down();
doremir_event_t doremir_system_event_key_up();
doremir_event_t doremir_system_event_key_down();
doremir_event_t doremir_system_event_select(doremir_list_t);
doremir_message_sender_t doremir_system_event_select_sender(doremir_list_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_SYSTEM_EVENT

