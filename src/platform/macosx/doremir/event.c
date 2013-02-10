
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/string.h>
#include <doremir/thread.h>

#define NO_THREAD_T
#include <doremir/util.h>

#include <ApplicationServices/ApplicationServices.h>


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
            printf("    Unicode char: %d\n", cs[0]);
            string_t str = doremir_string_from_utf16(cs);
            printf("    Unicode string: %s\n", unstring(str));

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
        | CGEventMaskBit(kCGEventLeftMouseDown) 
        | CGEventMaskBit(kCGEventKeyDown)
        | CGEventMaskBit(kCGEventLeftMouseDragged)
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
}

void test_hid()
{
    doremir_thread_create(test_hid2, NULL);
    printf("Launching monitor thread\n");
    while(1)
        doremir_thread_sleep(1000);
    printf("Returning\n");
}    