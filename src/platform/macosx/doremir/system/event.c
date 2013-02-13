
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/atomic.h>
#include <doremir/string.h>
#include <doremir/message.h>
#include <doremir/event.h>
#include <doremir/thread.h>
#include <doremir/system/event.h>

#define NO_THREAD_T
#include <doremir/util.h>
#undef NO_THREAD_T

#include <ApplicationServices/ApplicationServices.h>

/*
    Notes:
        - Using the CoreGraphics Event API
        - Might also use IOKit
        - Can not get keyboard events unless 'Access for assistive devices' is enabled
 */

doremir_event_t doremir_system_event_mouse_move()
{
    return doremir_system_event_select(list(i32(mouse_move_event)));
}

doremir_event_t doremir_system_event_mouse_drag()
{
    return doremir_system_event_select(list(i32(mouse_drag_event)));
}

doremir_event_t doremir_system_event_mouse_up()
{
    return doremir_system_event_select(list(i32(mouse_up_event)));
}

doremir_event_t doremir_system_event_mouse_down()
{
    return doremir_system_event_select(list(i32(mouse_down_event)));
}

doremir_event_t doremir_system_event_key_up()
{
    return doremir_system_event_select(list(i32(key_up_event)));
}

doremir_event_t doremir_system_event_key_down()
{
    return doremir_system_event_select(list(i32(key_down_event)));
}


/** Returns a system event from the given selectors.

    @param sources  A list of @ref doremir_system_event_source_t (destroyed).
    @return         A new system event.
 */
doremir_event_t doremir_system_event_select(doremir_list_t sources)
{
    doremir_message_sender_t source = doremir_system_event_send(sources);
    doremir_destroy(sources);
    return doremir_event_receive(source, i16(0));
}


struct event_disp {
    impl_t              impl;           // Implementations

    CGEventMask         mask;
    dispatcher_t        disp;           // Outgoing event dispatcher

    doremir_thread_t    thread;         // Thread
    CFRunLoopRef        loop;           // Run loop
    atomic_t            loop_set;
};

typedef struct event_disp           *event_disp_t;
typedef doremir_system_event_source_t  event_source_t;

ptr_t event_disp_impl(doremir_id_t interface);


inline static ptr_t convert_event(CGEventType type, CGEventRef event)
{
    switch (type) {
    case kCGEventKeyUp: {
        int keyCode = CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);

        UniCharCount sz;
        UniChar      cs[11];
        CGEventKeyboardGetUnicodeString(event, 10, &sz, cs);
        cs[sz] = 0;

        if (sz >= 1) {
            string_t str = doremir_string_single(cs[0]);
            return list(i16(keyCode), i16(cs[0]), str);
        } else {
            return list(i16(keyCode), i16(0), string(""));
        }

        break;
    }

    case kCGEventKeyDown: {
        int keyCode = CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);

        UniCharCount sz;
        UniChar      cs[11];
        CGEventKeyboardGetUnicodeString(event, 10, &sz, cs);
        cs[sz] = 0;

        if (sz >= 1) {
            string_t str = doremir_string_single(cs[0]);
            return list(i16(keyCode), i16(cs[0]), str);
        } else {
            return list(i16(keyCode), i16(0), string(""));
        }

        break;
    }

    case kCGEventMouseMoved: {
        CGPoint point = CGEventGetLocation(event);
        return pair(f32(point.x), f32(point.y));
    }

    case kCGEventLeftMouseUp: {
        CGPoint point = CGEventGetLocation(event);
        return pair(f32(point.x), f32(point.y));
    }

    case kCGEventLeftMouseDown: {
        CGPoint point = CGEventGetLocation(event);
        return pair(f32(point.x), f32(point.y));
    }

    case kCGEventLeftMouseDragged: {
        CGPoint point = CGEventGetLocation(event);
        return pair(f32(point.x), f32(point.y));
    }
    }

    assert(false && "Unreachable");
}

static CGEventRef event_listener(CGEventTapProxy proxy,
                                 CGEventType     type,
                                 CGEventRef      event,
                                 void           *data)
{
    event_disp_t  disp    = data;

    // printf("Event of type %d\n", type);
    ptr_t value = convert_event(type, event);

    doremir_message_send(disp->disp, i16(0), value);

    return event;
}


static ptr_t add_event_listener(ptr_t a)
{
    event_disp_t  disp    = a;
    CFMachPortRef   eventTap  = CGEventTapCreate(kCGSessionEventTap,
                                                 kCGTailAppendEventTap,
                                                 kCGEventTapOptionListenOnly,
                                                 disp->mask,
                                                 (CGEventTapCallBack) event_listener, disp);

    assert(eventTap && "No eventTap");

    CFRunLoopSourceRef runLoopSource = CFMachPortCreateRunLoopSource(kCFAllocatorDefault,
                                       eventTap, 0);
    assert(runLoopSource && "No runLoopSource");

    CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, kCFRunLoopCommonModes);
    CGEventTapEnable(eventTap, true);

    disp->loop = CFRunLoopGetCurrent();
    doremir_atomic_set(disp->loop_set, (ptr_t) 1);

    printf("Entering loop");
    CFRunLoopRun();

    return 0;
}

inline static CGEventMask convert_source(event_source_t type)
{
    match(type) {
        against(mouse_move_event)   CGEventMaskBit(kCGEventMouseMoved);
        against(mouse_drag_event)   CGEventMaskBit(kCGEventLeftMouseDragged);
        against(mouse_up_event)     CGEventMaskBit(kCGEventLeftMouseUp);
        against(mouse_down_event)   CGEventMaskBit(kCGEventLeftMouseDown);
        against(key_up_event)       CGEventMaskBit(kCGEventKeyUp);
        against(key_down_event)     CGEventMaskBit(kCGEventKeyDown);
        no_default();
    }
}

/** Returns a sender that sends a message whenever the given system event occurs.

    The returned value implements [Sender](@ref doremir_message_sender_t) and
    [Destroy](@ref doremir_destroy_t), and should be destroyed after use.

    @param sources  A list of @ref doremir_system_event_source_t (destroyed).
    @return         A new sender.
 */
doremir_message_sender_t doremir_system_event_send(doremir_list_t sources)
{
    CGEventMask mask = 0;
    doremir_for_each(source, sources) {
        mask |= convert_source(ti16(source));
    }

    event_disp_t disp = doremir_new_struct(event_disp);
    disp->impl        = &event_disp_impl;

    disp->mask        = mask;
    disp->disp        = lockfree_dispatcher();
    disp->loop        = NULL;                     // Set by new thread
    disp->loop_set    = atomic();

    disp->thread      = doremir_thread_create(add_event_listener, disp);

    // Wait until run loop has been set
    while (!doremir_atomic_get(disp->loop_set)) {
        doremir_thread_sleep(1);
    }

    return (doremir_message_sender_t) disp;
}


void event_disp_destroy(ptr_t a)
{
    event_disp_t disp = a;

    doremir_destroy(disp->disp);
    doremir_destroy(disp->loop_set);

    CFRunLoopStop(disp->loop);
    doremir_thread_join(disp->thread);

    doremir_delete(disp);
}


void event_disp_sync(ptr_t a)
{
    event_disp_t disp = a;
    doremir_message_sync(disp->disp);
}

doremir_list_t event_disp_receive(ptr_t a, address_t addr)
{
    event_disp_t disp = a;
    return doremir_message_receive(disp->disp, addr);
}


ptr_t event_disp_impl(doremir_id_t interface)
{
    static doremir_destroy_t event_disp_destroy_impl
        = { event_disp_destroy };
    static doremir_message_sender_interface_t event_disp_message_sender_interface_impl
        = { event_disp_sync, event_disp_receive };

    switch (interface) {

    case doremir_destroy_i:
        return &event_disp_destroy_impl;

    case doremir_message_sender_interface_i:
        return &event_disp_message_sender_interface_impl;

    default:
        return NULL;
    }
}


