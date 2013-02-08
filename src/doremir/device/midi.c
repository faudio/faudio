
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

session_t doremir_device_midi_begin_session()
{
}

void doremir_device_midi_end_session(session_t session)
{
}

void doremir_device_midi_with_session(session_callback_t    session_callback,
                                      doremir_ptr_t         session_data,
                                      error_callback_t      error_callback,
                                      doremir_ptr_t         error_data)
{
}

doremir_list_t doremir_device_midi_all(session_t session)
{
}

doremir_pair_t doremir_device_midi_default(session_t session)
{
}

device_t doremir_device_midi_default_input(session_t session)
{
}

device_t doremir_device_midi_default_output(session_t session)
{
}

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

doremir_string_t doremir_device_midi_name(device_t device)
{
}

doremir_string_t doremir_device_midi_host_name(device_t device)
{
}

bool doremir_device_midi_has_input(device_t device)
{
}

bool doremir_device_midi_has_output(device_t device)
{
}

doremir_device_midi_stream_t doremir_device_midi_open_stream(device_t device)
{
}


void doremir_device_midi_close_stream(stream_t stream)
{
}


void doremir_device_midi_with_stream(device_t           device,
                                     stream_callback_t  stream_callback,
                                     doremir_ptr_t      stream_data,
                                     error_callback_t   error_callback,
                                     doremir_ptr_t      error_data)
{
}







// --------------------------------------------------------------------------------

doremir_string_t midi_session_show(ptr_t a)
{
    // string_t str = string("<AudioSession ");
    // str = string_dappend(str, doremir_string_format_integer(" %p", (long) a));
    // str = string_dappend(str, string(">"));
    // return str;
}

void midi_session_destroy(ptr_t a)
{
    // doremir_device_midi_end_session(a);
}

ptr_t midi_session_impl(doremir_id_t interface)
{
    static doremir_string_show_t midi_session_show_impl
        = { midi_session_show };
    static doremir_destroy_t midi_session_destroy_impl
        = { midi_session_destroy };

    switch (interface) {
    case doremir_string_show_i:
        return &midi_session_show_impl;

    case doremir_destroy_i:
        return &midi_session_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool midi_device_equal(ptr_t a, ptr_t b)
{
    // device_t device1 = (device_t) a;
    // device_t device2 = (device_t) b;
    // // TODO check that session is valid
    // return device1->index == device2->index;
}

doremir_string_t midi_device_show(ptr_t a)
{
    device_t device = (device_t) a;

    // string_t str = string("<AudioDevice ");
    // str = string_dappend(str, doremir_device_midi_host_name(device));
    // str = string_dappend(str, string(" "));
    // str = string_dappend(str, doremir_device_midi_name(device));
    // str = string_dappend(str, string(">"));
    // return str;
}

ptr_t midi_device_impl(doremir_id_t interface)
{
    static doremir_equal_t midi_device_equal_impl
        = { midi_device_equal };
    static doremir_string_show_t midi_device_show_impl
        = { midi_device_show };

    switch (interface) {
    case doremir_equal_i:
        return &midi_device_equal_impl;

    case doremir_string_show_i:
        return &midi_device_show_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

doremir_string_t midi_stream_show(ptr_t a)
{
    string_t str = string("<AudioStream ");
    // str = string_dappend(str, doremir_string_format_integer(" %p", (long) a));
    // str = string_dappend(str, string(">"));
    return str;
}

void midi_stream_destroy(ptr_t a)
{
    doremir_device_midi_close_stream(a);
}

void midi_stream_sync(ptr_t a)
{
    stream_t stream = (stream_t) a;
    assert(false && "Not implemented");
}

doremir_list_t midi_stream_receive(ptr_t a, address_t addr)
{
    // stream_t stream = (stream_t) a;
    // assert(false && "Not implemented");
}

void midi_stream_send(ptr_t a, address_t addr, message_t msg)
{
    // stream_t stream = (stream_t) a;
    // doremir_message_send(stream->incoming, addr, msg);
}

ptr_t midi_stream_impl(doremir_id_t interface)
{
    static doremir_string_show_t midi_stream_show_impl
        = { midi_stream_show };
    static doremir_destroy_t midi_stream_destroy_impl
        = { midi_stream_destroy };
    static doremir_message_receiver_t midi_stream_message_receiver_impl
        = { midi_stream_send };
    static doremir_message_sender_t midi_stream_message_sender_impl
        = { midi_stream_sync, midi_stream_receive };

    switch (interface) {


    case doremir_string_show_i:
        return &midi_stream_show_impl;

    case doremir_destroy_i:
        return &midi_stream_destroy_impl;

    case doremir_message_sender_i:
        return &midi_stream_message_sender_impl;

    case doremir_message_receiver_i:
        return &midi_stream_message_receiver_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

void doremir_audio_engine_log_error_from(doremir_string_t msg, doremir_string_t origin);

error_t midi_device_error(string_t msg)
{
    return doremir_error_create_simple(error,
                                       msg,
                                       string("Doremir.Device.Audio"));
}

error_t midi_device_error_with(string_t msg, int code)
{
    return doremir_error_create_simple(error,
                                       string_dappend(msg, format_integer(" (error code %d)", code)),
                                       string("Doremir.Device.Audio"));
}

void midi_device_fatal(string_t msg, int code)
{
    doremir_audio_engine_log_error_from(
        string_dappend(msg, format_integer(" (error code %d)", code)),
        string("Doremir.Device.Audio"));

    doremir_audio_engine_log_error(string("Terminating Audio Engine"));
    exit(error);
}

