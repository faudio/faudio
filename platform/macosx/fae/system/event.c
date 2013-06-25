
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/atomic.h>
#include <fae/string.h>
#include <fae/message.h>
#include <fae/event.h>
#include <fae/thread.h>
#include <fae/system/event.h>

#define NO_THREAD_T
#include <fae/util.h>
#undef NO_THREAD_T

#include <ApplicationServices/ApplicationServices.h>

/*
    Notes:
        - Using the CoreGraphics Event API
        - Might also use IOKit
        - Can not get keyboard events unless 'Access for assistive devices' is enabled
 */

fae_event_t fae_system_event_mouse_move()
{
    return fae_system_event_select(list(i16(mouse_move_event)));
}

fae_event_t fae_system_event_mouse_drag()
{
    return fae_system_event_select(list(i16(mouse_drag_event)));
}

fae_event_t fae_system_event_mouse_up()
{
    return fae_system_event_select(list(i16(mouse_up_event)));
}

fae_event_t fae_system_event_mouse_down()
{
    return fae_system_event_select(list(i16(mouse_down_event)));
}

fae_event_t fae_system_event_key_up()
{
    return fae_system_event_select(list(i16(key_up_event)));
}

fae_event_t fae_system_event_key_down()
{
    return fae_system_event_select(list(i16(key_down_event)));
}


/** Returns an event selected from the given types.

    @param types      A list of @ref fae_system_event_type_t (destroyed).
    @return             A new system event.
 */
fae_event_t fae_system_event_select(fae_list_t types)
{
    fae_message_sender_t type = fae_system_event_receive(types);
    fae_destroy(types);
    return fae_event_receive(type, i16(0));
}

/** Transforms the given event to write its values to the standard output.
    @param event        An event of strings.
    @return             An event of null values.
 */
fae_event_t fae_system_event_write_std(fae_event_t event)
{
    return fae_event_send(fae_system_event_send_std(), i16(0), event);
}

/** Transforms the given event to write its values to the log.
    @param event        An event of strings.
    @return             An event of null values.
 */
fae_event_t fae_system_event_write_log(fae_event_t event)
{
    return fae_event_send(fae_system_event_send_log(), i16(0), event);
}


// --------------------------------------------------------------------------------

struct event_source {
    impl_t              impl;           // Implementations

    CGEventMask         mask;
    dispatcher_t        disp;           // Outgoing event dispatcher

    fae_thread_t    thread;         // Thread
    CFRunLoopRef        loop;           // Run loop
    atomic_t            loop_set;
};

typedef fae_system_event_type_t  event_type_t;
typedef struct event_source              *event_source_t;

ptr_t event_source_impl(fae_id_t interface);


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
            string_t str = fae_string_single(cs[0]);
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
            string_t str = fae_string_single(cs[0]);
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

    fae_message_send((receiver_t) source->disp, i16(0), value);

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
    fae_atomic_set(source->loop_set, (ptr_t) 1);

    // printf("Entering loop");
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

/** Returns a sender of values selected from the given types.

    The returned value implements [Sender](@ref fae_message_sender_t) and
    [Destroy](@ref fae_destroy_t), and should be destroyed after use.

    @param types    A list of @ref fae_system_event_type_t (destroyed).
    @return         A new sender.
 */
fae_message_sender_t fae_system_event_receive(fae_list_t types)
{
    CGEventMask mask = 0;
    fae_for_each(type, types) {
        mask |= convert_type(ti16(type));
    }

    event_source_t source = fae_new_struct(event_source);
    source->impl        = &event_source_impl;

    source->mask        = mask;
    source->disp        = lockfree_dispatcher();
    source->loop        = NULL;                     // Set by new thread
    source->loop_set    = atomic();

    source->thread      = fae_thread_create(add_event_listener, source);

    // Wait until run loop has been set
    while (!fae_atomic_get(source->loop_set)) {
        fae_thread_sleep(1);
    }

    return (fae_message_sender_t) source;
}


void event_source_destroy(ptr_t a)
{
    event_source_t source = a;

    fae_destroy(source->disp);
    fae_destroy(source->loop_set);

    CFRunLoopStop(source->loop);
    fae_thread_join(source->thread);

    fae_delete(source);
}

void event_source_sync(ptr_t a)
{
    event_source_t source = a;
    fae_message_sync((sender_t) source->disp);
}

fae_list_t event_source_receive(ptr_t a, address_t addr)
{
    event_source_t source = a;
    return fae_message_receive((sender_t) source->disp, addr);
}

ptr_t event_source_impl(fae_id_t interface)
{
    static fae_destroy_t event_source_destroy_impl
        = { event_source_destroy };
    static fae_message_sender_interface_t event_source_message_sender_interface_impl
        = { event_source_sync, event_source_receive };

    switch (interface) {

    case fae_destroy_i:
        return &event_source_destroy_impl;

    case fae_message_sender_interface_i:
        return &event_source_message_sender_interface_impl;

    default:
        return NULL;
    }
}

// --------------------------------------------------------------------------------


// TODO we probably want this to contain file handle or libuv equivalent

struct io_event_sink {
    impl_t              impl;           // Implementations
};

typedef struct io_event_sink             *io_event_sink_t;

ptr_t io_event_sink_impl(fae_id_t interface);

fae_message_receiver_t fae_system_event_send_std()
{
    io_event_sink_t sink = fae_new_struct(io_event_sink);
    sink->impl  = &io_event_sink_impl;
    // TODO

    return (fae_message_receiver_t) sink;
}

void io_event_sink_destroy(ptr_t a)
{
    io_event_sink_t sink = a;
    fae_delete(sink);
}

void io_event_sink_send(fae_ptr_t a, fae_message_address_t addr, fae_message_t msg)
{
    // sink and addr ignored
    string_t str = fae_string_to_string(msg);
    printf("--------------------------------------------------------------------------------> %s\n", unstring(str));
    fflush(stdout);
    fae_destroy(str);
}

ptr_t io_event_sink_impl(fae_id_t interface)
{
    static fae_destroy_t io_event_sink_destroy_impl
        = { io_event_sink_destroy };
    static fae_message_receiver_interface_t io_event_sink_message_receiver_interface_impl
        = { io_event_sink_send };

    switch (interface) {

    case fae_destroy_i:
        return &io_event_sink_destroy_impl;

    case fae_message_receiver_interface_i:
        return &io_event_sink_message_receiver_interface_impl;

    default:
        return NULL;
    }
}

// --------------------------------------------------------------------------------


struct log_event_sink {
    impl_t              impl;           // Implementations
};

typedef struct log_event_sink             *log_event_sink_t;

ptr_t log_event_sink_impl(fae_id_t interface);

fae_message_receiver_t fae_system_event_send_log()
{
    log_event_sink_t sink = fae_new_struct(log_event_sink);
    sink->impl  = &log_event_sink_impl;
    return (fae_message_receiver_t) sink;
}

void log_event_sink_destroy(ptr_t a)
{
    log_event_sink_t sink = a;
    fae_delete(sink);
}

void log_event_sink_send(fae_ptr_t a, fae_message_address_t addr, fae_message_t msg)
{
    // sink and addr ignored
    inform(fae_string_show(msg));
}

ptr_t log_event_sink_impl(fae_id_t interface)
{
    static fae_destroy_t log_event_sink_destroy_impl
        = { log_event_sink_destroy };
    static fae_message_receiver_interface_t log_event_sink_message_receiver_interface_impl
        = { log_event_sink_send };

    switch (interface) {

    case fae_destroy_i:
        return &log_event_sink_destroy_impl;

    case fae_message_receiver_interface_i:
        return &log_event_sink_message_receiver_interface_impl;

    default:
        return NULL;
    }
}