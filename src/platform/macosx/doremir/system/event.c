
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

    // Note: Can not get keyboard events unless 'Access for assistive devices' is enabled


static CGEventRef eventTapFunction(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *refcon)
{
    printf("Event of type %d\n", type);

    switch(type)
    {
        case kCGEventKeyUp: {
            int keyCode = CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
            printf("    Key code: %d\n", keyCode);

            UniCharCount sz;
            UniChar      cs[11];
            CGEventKeyboardGetUnicodeString(event, 10, &sz, cs);
            cs[sz] = 0;
            printf("    Unicode size: %d\n", sz);
            string_t str = doremir_string_from_utf16(cs);
            printf("    Unicode string: %s\n", unstring(str));

            break;
        }
        case kCGEventKeyDown: {
            int keyCode = CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
            printf("    Key code: %d\n", keyCode);

            UniCharCount sz;
            UniChar      cs[11];
            CGEventKeyboardGetUnicodeString(event, 10, &sz, cs);
            cs[sz] = 0;
            printf("    Unicode size: %d\n", sz);

            if (sz >= 1)
            {
                string_t str = doremir_string_single(cs[0]);
                printf("    Unicode char:   %d\n", cs[0]);
                printf("    Unicode string: %s\n", unstring(str));
                doremir_destroy(str);
            }
            else
            {
                printf("    Unicode char:   n/a\n");
                printf("    Unicode string: n/a\n");
            }

            break;
        }

        case kCGEventMouseMoved: {
            CGPoint point = CGEventGetLocation(event);
            printf("    Location: (%f, %f)\n", point.x, point.y);
            break;
        }
        case kCGEventLeftMouseUp: {
            CGPoint point = CGEventGetLocation(event);
            printf("    Location: (%f, %f)\n", point.x, point.y);
            break;
        }
        case kCGEventLeftMouseDown: {
            CGPoint point = CGEventGetLocation(event);
            printf("    Location: (%f, %f)\n", point.x, point.y);
            break;
        }
        case kCGEventLeftMouseDragged: {
            CGPoint point = CGEventGetLocation(event);
            printf("    Location: (%f, %f)\n", point.x, point.y);
            break;
        }
    }


    return event;
}

ptr_t test_hid2(ptr_t _)
{
    ProcessSerialNumber currentProcess;
    GetCurrentProcess(&currentProcess);
    CFMachPortRef      eventTap;
    CFRunLoopSourceRef runLoopSource;

    CGEventMask eventMask = 0
        | CGEventMaskBit(kCGEventMouseMoved)
        | CGEventMaskBit(kCGEventLeftMouseDown)
        // | CGEventMaskBit(kCGEventKeyDown)
        // | CGEventMaskBit(kCGEventLeftMouseDragged)
        ;

    // Can not get keyboard events unless 'Access for assistive devices' is enabled
    eventTap = CGEventTapCreate(
        kCGSessionEventTap,
        kCGTailAppendEventTap,          // kCGHeadInsertEventTap
        kCGEventTapOptionListenOnly,    // kCGEventTapOptionDefault
        eventMask,
        (CGEventTapCallBack)eventTapFunction,
        NULL
        );
    assert(eventTap && "No eventTap");

    // Create a run loop source.
    runLoopSource = CFMachPortCreateRunLoopSource(
                        kCFAllocatorDefault, eventTap, 0);
    assert(runLoopSource && "No runLoopSource");

    CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, kCFRunLoopCommonModes);
    CGEventTapEnable(eventTap, true);
    CFRunLoopRun();

    return 0;
}

void test_hid()
{
    doremir_thread_create(test_hid2, NULL);
    printf("Launching monitor thread\n");
    while(1)
        doremir_thread_sleep(1000);
    printf("Returning\n");
}

















struct event_source {
    impl_t          impl;           // Implementations

    CGEventMask     mask;
    dispatcher_t    disp;           // Outgoing event dispatcher

    thread_t        thread;         // Thread
    CFRunLoopRef    loop;           // Run loop
    atomic_t        loop_set;
};

typedef struct event_source* event_source_t;
typedef doremir_system_event_type_t event_type_t;

ptr_t event_source_impl(doremir_id_t interface);

static CGEventRef event_listener(CGEventTapProxy proxy,
                                 CGEventType type, 
                                 CGEventRef event, 
                                 void *data)
{
    printf("Event of type %d\n", type);
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

    CFRunLoopRun();
}

inline static CGEventMask convert_type(event_type_t type)
{
    match (type) {
        against(mouse_move_event)
            CGEventMaskBit(kCGEventMouseMoved);
        against(mouse_drag_event)
            CGEventMaskBit(kCGEventLeftMouseDragged);
        against(mouse_up_event)
            CGEventMaskBit(kCGEventLeftMouseUp);
        against(mouse_down_event)
            CGEventMaskBit(kCGEventLeftMouseDown);
        against(key_up_event)
            CGEventMaskBit(kCGEventKeyUp);
        against(key_down_event)
            CGEventMaskBit(kCGEventKeyDown);
        no_default();
    }
}

/** Returns a sender for mouse and keyboard events.

    The returned value implements [Sender](@ref doremir_message_sender_t) and
    [Destroy](@ref doermir_destroy_t), and should be destroyed after use.

 */
doremir_message_some_sender_t doremir_system_event_get_sender(doremir_list_t sources)
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
    while (!doremir_atomic_get(source->loop_set))
        doremir_thread_sleep(1);

    return source;
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
}

doremir_list_t event_source_receive(ptr_t a, address_t addr)
{
    event_source_t source = a;
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









/** Returns a sender for mouse and keyboard events.

 */
doremir_event_t doremir_system_event_get_event(doremir_list_t sources)
{

}

