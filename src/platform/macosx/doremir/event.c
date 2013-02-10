
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/string.h>
// #include <doremir/thread.h>
#include <doremir/util.h>

// #include <IOKit/IOTypes.h>
// #include <IOKit/IOReturn.h>
// #include <IOKit/hid/IOHIDLib.h>
// #include <CoreFoundation/CoreFoundation.h>

#include <ApplicationServices/ApplicationServices.h>


static int count_g = 0;
static CGEventRef eventTapFunction(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *refcon)
{
    count_g++;   
    printf("Event of type %d\n", type);
    return event;
}

ptr_t test_hid2(ptr_t _)
{

    ProcessSerialNumber currentProcess;
    GetCurrentProcess(&currentProcess);
    CFMachPortRef      eventTap;
    CGEventMask        eventMask;
    CFRunLoopSourceRef runLoopSource;
 
    CGEventMask mask = 0 | CGEventMaskBit(kCGEventLeftMouseDown);
    
    // // Only works if we are root, OR if 'Access for assistive devices' is enabled in System Preferences
    eventTap = CGEventTapCreate(
        kCGSessionEventTap,
        kCGTailAppendEventTap,          // kCGHeadInsertEventTap
        kCGEventTapOptionListenOnly,    // kCGEventTapOptionDefault
        mask,
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

    printf("Number of events: %d\n", count_g);
}

void test_hid()
{
    doremir_thread_create(test_hid2, NULL);
    printf("Launching monitor thread\n");
    doremir_thread_sleep(10000);
    printf("Returning\n");
}