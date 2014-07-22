
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

// --------------------------------------------------------------------------------

void fa_device_initialize()
{
    // Nothing
}

void fa_device_terminate()
{
    // Nothing
}

// --------------------------------------------------------------------------------



OSStatus audio_listener(AudioObjectID                       id,
                        UInt32                              size,
                        const AudioObjectPropertyAddress    addresses[],
                        void                                *data)
{
    OSStatus result;
    UInt32 propDataSize;
    fa_pair_t closure = data;

    result = AudioObjectGetPropertyDataSize(kAudioObjectSystemObject, addresses, 0, NULL, &propDataSize);

    int count = -1;

    if (result == noErr) {
        count = propDataSize / sizeof(AudioDeviceID);
    }

    fa_unpair(closure, function, data) {
        fa_nullary_t function2 = function;
        function2(data);
    }
    return noErr;
}

void add_audio_status_listener(fa_pair_t closure)
{
    OSStatus result;
    CFRunLoopRef theRunLoop;

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

    // Set runLoop to NULL
    // TODO Is this necessary?
    theRunLoop = NULL;
    result = AudioObjectSetPropertyData(kAudioObjectSystemObject, &runLoop, 0, NULL, sizeof(CFRunLoopRef), &theRunLoop);
    assert(result == noErr);

    result = AudioObjectAddPropertyListener(kAudioObjectSystemObject, &devices, audio_listener, closure);
    assert(result == noErr);
}

void remove_audio_status_listener(fa_pair_t closure)
{
    OSStatus result;

    AudioObjectPropertyAddress devices = {
        .mSelector = kAudioHardwarePropertyDevices,
        .mScope    = kAudioObjectPropertyScopeGlobal,
        .mElement  = kAudioObjectPropertyElementMaster
    };

    result = AudioObjectRemovePropertyListener(kAudioObjectSystemObject, &devices, audio_listener, closure);
    assert(result == noErr);
}


/*
    This function does nothing on OS X, as CoreMIDI handles hot-pluggning by itself.
    
    It is defined here in case we want to try and compile using the PortMIDI backend instead
    of the usual CoreMIDI implementation, in which case hog-plugging is not supported
    and the fa_audio_add_status_callback function has no effect.
*/
void add_midi_status_listener(midi_status_callback_t function, fa_ptr_t data)
{
}


