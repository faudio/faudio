
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/device/audio.h>
#include <doremir/device/midi.h>
#include <doremir/string.h>
#include <doremir/thread.h>
#include <doremir/util.h>

#include <CoreAudio/AudioHardware.h>
#include <CoreMidi/MIDIServices.h>

/*
    Device detection for OS X
    Used by implementation of the Device.Audio and Device.Midi modules.

    TODO remove added listeners?
 */
typedef doremir_device_audio_status_callback_t  audio_status_callback_t;
typedef doremir_device_midi_status_callback_t   midi_status_callback_t;

struct nullary_closure {
    nullary_t   function;
    ptr_t       data;
};
typedef struct nullary_closure *closure_t;

inline static closure_t new_closure(nullary_t function, ptr_t data)
{
    closure_t closure = malloc(sizeof(struct nullary_closure));
    closure->function = function;
    closure->data = data;
    return closure;
}

OSStatus audio_listener(AudioObjectID                       id,
                        UInt32                              size,
                        const AudioObjectPropertyAddress    addresses[],
                        void                                *data)
{
    OSStatus result;
    UInt32 propDataSize;
    closure_t closure = data;

    result = AudioObjectGetPropertyDataSize(kAudioObjectSystemObject, addresses, 0, NULL, &propDataSize);

    int count = -1;

    if (result == noErr) {
        count = propDataSize / sizeof(AudioDeviceID);
    }

    closure->function(closure->data);
    return noErr;
}

void add_audio_status_listener(audio_status_callback_t function, ptr_t data)
{
    OSStatus result;
    CFRunLoopRef theRunLoop;
    closure_t closure;

    AudioObjectPropertyAddress runLoop = {
        .mSelector = kAudioHardwarePropertyRunLoop,
        .mScope    = kAudioObjectPropertyScopeGlobal,
        .mElement  = kAudioObjectPropertyElementMaster
    };

    AudioObjectPropertyAddress devices = {
        .mSelector = kAudioHardwarePropertyDevices,
        .mScope    = kAudioObjectPropertyScopeGlobal,
        .mElement  = kAudioObjectPropertyElementMaster
    };

    theRunLoop = NULL; // necessary?
    result = AudioObjectSetPropertyData(kAudioObjectSystemObject, &runLoop, 0, NULL, sizeof(CFRunLoopRef), &theRunLoop);
    assert(result == noErr);

    closure = new_closure(function, data);
    result = AudioObjectAddPropertyListener(kAudioObjectSystemObject, &devices, audio_listener, closure);
    assert(result == noErr);
}


void midi_listener(const MIDINotification *message, void *data)
{
    MIDINotificationMessageID id = message->messageID;
    UInt32                    sz = message->messageSize;
    printf("id: %d, size: %d\n", id, sz);

    /*
    enum { // MIDINotificationMessageID
        kMIDIMsgSetupChanged = 1,
        kMIDIMsgObjectAdded = 2,
        kMIDIMsgObjectRemoved = 3,
        kMIDIMsgPropertyChanged = 4,
        kMIDIMsgThruConnectionsChanged = 5,
        kMIDIMsgSerialPortOwnerChanged = 6,
        kMIDIMsgIOError = 7
    };
    */
    if (id == kMIDIMsgSetupChanged)
    {
        closure_t closure = data;
        closure->function(closure->data);
    }
}

// From https://ccrma.stanford.edu/~craig/articles/linuxmidi/osxmidi/testout.c
//
//      "Note that notifyProc will always be called on the run loop 
//      which was current when MIDIClientCreate was first called."

// See http://lists.apple.com/archives/coreaudio-api/2002/Feb/msg00180.html
// http://comelearncocoawithme.blogspot.se/2011/08/reading-from-external-controllers-with.html
// void
// MyTimerCallback(CFRunLoopTimerRef timer, void *info)
// {
//     printf("Called! Devs: %d\n",  MIDIGetNumberOfSources());
// }

static CFRunLoopSourceRef source = NULL;
void midi_listener_loop(closure_t closure)
{
    OSStatus result;
    CFStringRef name;
    MIDIClientRef client;

    name = doremir_string_to_cf_string(string("DoReMIRAudioxx"));
    result = MIDIClientCreate(name, midi_listener, closure, &client);
    assert(result == noErr);

    

    // printf("Entering loop\n");
    // CFRunLoopRun();
    // printf("Exiting loop\n");
}

void add_midi_status_listener(midi_status_callback_t function, ptr_t data)
{
    closure_t closure;
    closure = new_closure(function, data);

    // pid_t pid;
    // if ((pid = fork()) < 0)
    //         {
    //             assert(false && "No fork");
    //         }
    //         else if (pid == 0)
    //         {
    //             // return
    //         }
    //         else
    //         {
    //             midi_listener_loop(closure);
    //         }
    
    midi_listener_loop(closure);
    
    
    // thread_t thread = doremir_thread_create(midi_listener_loop, closure);
    // doremir_thread_detach(thread); // TODO safe?
    
    printf("Exited add\n");
}



