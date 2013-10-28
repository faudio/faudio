
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/audio.h>
#include <fa/midi.h>
#include <fa/string.h>
#include <fa/thread.h>
#include <fa/util.h>

#include <CoreAudio/AudioHardware.h>
#include <CoreMidi/MIDIServices.h>

typedef fa_audio_status_callback_t  audio_status_callback_t;
typedef fa_midi_status_callback_t   midi_status_callback_t;

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

void add_midi_status_listener(midi_status_callback_t function, ptr_t data)
{
    // Note: This function does nothing on OS X

    // It is only here if one wants to try and compile using the PortMIDI backend instead
    // of the usual CoreMIDI implementation, in which case hog-plugging is not supported
    // and the fa_audio_add_status_callback function has no effect.
}


