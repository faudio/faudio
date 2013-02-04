
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/device/audio.h>
#include <doremir/string.h>
#include <doremir/util.h>

#include <CoreAudio/AudioHardware.h>

typedef doremir_device_audio_t                  device_t;
typedef doremir_device_audio_stream_t           stream_t;
typedef doremir_device_audio_session_t          session_t;
typedef doremir_device_audio_stream_callback_t  stream_callback_t;
typedef doremir_device_audio_session_callback_t session_callback_t;
typedef doremir_device_audio_status_callback_t  status_callback_t;


struct callback_closure {
    nullary_t   func;
    ptr_t       data;
};

OSStatus listener(
    AudioObjectID     object_id,
    UInt32            num_addresses,
    const AudioObjectPropertyAddress addresses[],
    void *data)
{
    OSStatus result;
    UInt32 propDataSize;

    result = AudioObjectGetPropertyDataSize(kAudioObjectSystemObject, addresses, 0, NULL, &propDataSize);

    int count = -1;

    if (result == noErr) {
        count = propDataSize / sizeof(AudioDeviceID);
    }

    nullary_t func  = ((struct callback_closure *) data)->func;
    nullary_t data2 = ((struct callback_closure *) data)->data;
    func(data2);

    return noErr;
}

void set_device_status_impl(status_callback_t function, ptr_t data)
{
    OSStatus result;
    CFRunLoopRef theRunLoop;
    struct callback_closure *closure;

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

    // TODO leaks
    closure = malloc(sizeof(struct callback_closure));
    closure->func = function;
    closure->data = data;

    result = AudioObjectSetPropertyData(kAudioObjectSystemObject, &runLoop, 0, NULL, sizeof(CFRunLoopRef), &theRunLoop);
    assert(result == noErr);

    result = AudioObjectAddPropertyListener(kAudioObjectSystemObject, &devices, listener, closure);
    assert(result == noErr);
}

