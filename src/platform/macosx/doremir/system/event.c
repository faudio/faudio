
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

/**
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


/** Returns an event selected from the given sources.
    @param sources  List of sources (destroyed).
 */
doremir_event_t doremir_system_event_select(doremir_list_t sources)
{
    doremir_message_some_sender_t source = doremir_system_event_select_sender(sources);
    doremir_destroy(sources);
    return doremir_event_receive(source, i16(0));
}


struct event_source {
    impl_t              impl;           // Implementations

    CGEventMask         mask;
    dispatcher_t        disp;           // Outgoing event dispatcher

    doremir_thread_t    thread;         // Thread
    CFRunLoopRef        loop;           // Run loop
    atomic_t            loop_set;
};

typedef struct event_source *event_source_t;
typedef doremir_system_event_type_t event_type_t;

ptr_t event_source_impl(doremir_id_t interface);


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
    event_source_t  source    = data;

    // printf("Event of type %d\n", type);
    ptr_t value = convert_event(type, event);

    doremir_message_send(source->disp, i16(0), value);

    return event;
}


static ptr_t add_event_listener(ptr_t a)
{
    event_source_t  source    = a;
    CFMachPortRef   eventTap  = CGEventTapCreate(kCGSessionEventTap,
                                                 kCGTailAppendEventTap,
                                                 kCGEventTapOptionListenOnly,
                                                 source->mask,
                                                 (CGEventTapCallBack) event_listener, source);

    assert(eventTap && "No eventTap");

    CFRunLoopSourceRef runLoopSource = CFMachPortCreateRunLoopSource(kCFAllocatorDefault,
                                       eventTap, 0);
    assert(runLoopSource && "No runLoopSource");

    CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, kCFRunLoopCommonModes);
    CGEventTapEnable(eventTap, true);

    source->loop = CFRunLoopGetCurrent();
    doremir_atomic_set(source->loop_set, 1);

    printf("Entering loop");
    CFRunLoopRun();

    return 0;
}

inline static CGEventMask convert_type(event_type_t type)
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

/** Returns a sender selected from the given sources.

    The returned value implements [Sender](@ref doremir_message_sender_t) and
    [Destroy](@ref doremir_destroy_t), and should be destroyed after use.

 */
doremir_message_some_sender_t doremir_system_event_select_sender(doremir_list_t sources)
{
    CGEventMask mask = 0;
    doremir_for_each(source, sources) {
        mask |= convert_type(ti16(source));
    }

    event_source_t source = doremir_new_struct(event_source);
    source->impl        = &event_source_impl;

    source->mask        = mask;
    source->disp        = lockfree_dispatcher();
    source->loop        = NULL;                     // Set by new thread
    source->loop_set    = atomic();

    source->thread      = doremir_thread_create(add_event_listener, source);

    // Wait until run loop has been set
    while (!doremir_atomic_get(source->loop_set)) {
        doremir_thread_sleep(1);
    }

    return (doremir_message_some_sender_t) source;
}


void event_source_destroy(ptr_t a)
{
    event_source_t source = a;

    doremir_destroy(source->disp);
    doremir_destroy(source->loop_set);

    CFRunLoopStop(source->loop);
    doremir_thread_join(source->thread);

    doremir_delete(source);
}


void event_source_sync(ptr_t a)
{
    event_source_t source = a;
    doremir_message_sync(source->disp);
}

doremir_list_t event_source_receive(ptr_t a, address_t addr)
{
    event_source_t source = a;
    return doremir_message_receive(source->disp, addr);
}


ptr_t event_source_impl(doremir_id_t interface)
{
    static doremir_destroy_t event_source_destroy_impl
        = { event_source_destroy };
    static doremir_message_sender_t event_source_message_sender_impl
        = { event_source_sync, event_source_receive };

    switch (interface) {

    case doremir_destroy_i:
        return &event_source_destroy_impl;

    case doremir_message_sender_i:
        return &event_source_message_sender_impl;

    default:
        return NULL;
    }
}


