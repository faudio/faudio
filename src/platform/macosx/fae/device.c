
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/device/audio.h>
#include <fae/device/midi.h>
#include <fae/string.h>
#include <fae/thread.h>
#include <fae/util.h>

#include <CoreAudio/AudioHardware.h>
#include <CoreMidi/MIDIServices.h>

/*
    Device detection for OS X
    Used by implementation of the Device.Audio and Device.Midi modules.

    TODO remove added listeners?
 */
typedef fae_device_audio_status_callback_t  audio_status_callback_t;
typedef fae_device_midi_status_callback_t   midi_status_callback_t;

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

    // UInt32                    sz = message->messageSize;
    if (id == kMIDIMsgSetupChanged) {
        closure_t closure = data;
        closure->function(closure->data);
    }
}

//  From https://ccrma.stanford.edu/~craig/articles/linuxmidi/osxmidi/testout.c
//
//              "Note that notifyProc will always be called on the run loop
//              which was current when MIDIClientCreate was first called."

// See also     http://lists.apple.com/archives/coreaudio-api/2002/Feb/msg00180.html
//              http://comelearncocoawithme.blogspot.se/2011/08/reading-from-external-controllers-with.html

// http://lists.apple.com/archives/coreaudio-api/2001/Nov/msg00087.

/*
    https://groups.google.com/forum/?fromgroups=#!topic/overtone/Rts-8g_rlR0

    In coremidi, this will work if & only if the device has previously been plugged
    in to that machine - ie not if it's a device the computer's never spoken to
    before.
 */

void midi_listener_loop(closure_t closure)
{
    OSStatus result;
    CFStringRef name;
    MIDIClientRef client;

    name = fae_string_to_cf_string(string("DoReMIRAudio"));
    result = MIDIClientCreate(name, midi_listener, closure, &client);
    // client is ignored
    assert(result == noErr);
}

void add_midi_status_listener(midi_status_callback_t function, ptr_t data)
{
    closure_t closure;
    closure = new_closure(function, data);

    // assert(fae_equal(fae_thread_main(), fae_thread_current())
    //        && "Must be run from main thread");
    if(fae_not_equal(fae_thread_main(), fae_thread_current()))
    {
        inform(string("Can not register midi status listerner for non-main thread."));
    }
    else
    {
        inform(string("Exited midi_listener_loop\n"));
        midi_listener_loop(closure);
        inform(string("Exited midi_listener_loop\n"));        
    }
    

}



