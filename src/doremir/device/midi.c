
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/device/midi.h>
#include <doremir/thread.h>
#include <doremir/util.h>

#include <portmidi.h>

/* 
    Notes:
        * Device detection handled by excplicit CoreMIDI/?? calls
        * Call MIDIRestart() ?
 */

typedef doremir_device_midi_t                  device_t;
typedef doremir_device_midi_stream_t           stream_t;
typedef doremir_device_midi_session_t          session_t;
typedef doremir_device_midi_stream_callback_t  stream_callback_t;
typedef doremir_device_midi_session_callback_t session_callback_t;
typedef doremir_device_midi_status_callback_t  status_callback_t;

// typedef PaDeviceIndex native_index_t;
// typedef PaStream     *native_stream_t;



void add_midi_status_listener(status_callback_t function, ptr_t data);

void doremir_device_midi_set_status_callback(
    status_callback_t function,
    ptr_t             data,
    session_t         session)
{
    assert(session && "Not a real session");

    // See device_osx.c and device_win.c
    add_midi_status_listener(function, data);
}
