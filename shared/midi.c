
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/midi.h>
#include <fa/midi/message.h>
#include <fa/list.h>
#include <fa/thread.h>
#include <fa/time.h>
#include <fa/util.h>

#include <portmidi.h>

/*
    ## Notes
    
    * Device detection handled by excplicit CoreMIDI/?? calls

    * Call MIDIRestart() ?

    * TODO proper error checking
 */

typedef fa_midi_device_t           device_t;
typedef fa_midi_stream_t           stream_t;
typedef fa_midi_session_t          session_t;
typedef fa_midi_stream_callback_t  stream_callback_t;
typedef fa_midi_session_callback_t session_callback_t;
typedef fa_midi_status_callback_t  status_callback_t;

typedef PmDeviceID    native_index_t;
typedef PmStream     *native_stream_t;

struct _fa_midi_session_t {

    impl_t              impl;               // Dispatcher
    system_time_t       acquired;           // Time of acquisition (not used at the moment)

    list_t              devices;            // Cached device list

    device_t            def_input;          // Default devices, both possibly null
    device_t            def_output;         // If present, these are also in the above list
};

struct _fa_midi_device_t {

    impl_t              impl;               // Dispatcher
    native_index_t      index;              // Native device index

    bool                input, output;      // Cached capabilities
    string_t            name;               // Cached names
    string_t            host_name;
};

struct _fa_midi_stream_t {

    impl_t              impl;               // Dispatcher
    native_stream_t     native_input,
                        native_output;      // Native stream
    device_t            device;
    list_t              incoming;
};

static mutex_t pm_mutex;
static bool    pm_status;

error_t midi_device_error(string_t msg);
error_t midi_device_error_with(string_t msg, int error);
error_t native_error(string_t msg, int code);
void midi_device_fatal(string_t msg, int code);
ptr_t midi_session_impl(fa_id_t interface);
ptr_t midi_device_impl(fa_id_t interface);
ptr_t midi_stream_impl(fa_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(native_index_t index);
inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t device);
inline static void delete_stream(stream_t stream);

long fa_midi_message_simple_to_long(fa_midi_message_t midi);


// --------------------------------------------------------------------------------

inline static session_t new_session()
{
    session_t session = fa_new(midi_session);
    session->impl = &midi_session_impl;
    return session;
}

inline static void session_init_devices(session_t session)
{
    native_index_t count;
    list_t         devices;

    count   = Pm_CountDevices();
    devices = fa_list_empty();

    for (size_t i = 0; i < count; ++i) {
        device_t device = new_device(i);

        if (device) {
            devices = fa_list_dcons(device, devices);
        }
    }

    session->devices      = fa_list_dreverse(devices);
    session->def_input    = new_device(Pm_GetDefaultInputDeviceID());
    session->def_output   = new_device(Pm_GetDefaultOutputDeviceID());
}

inline static void delete_session(session_t session)
{
    // TODO free device list
    fa_delete(session);
}

inline static device_t new_device(native_index_t index)
{
    if (index == pmNoDevice) {
        return NULL;
    }

    device_t device = fa_new(midi_device);
    device->impl    = &midi_device_impl;

    const PmDeviceInfo  *info = Pm_GetDeviceInfo(index);

    device->index       = index;
    device->input       = info->input;
    device->output      = info->output;
    device->name        = string((char *) info->name);      // const cast
    device->host_name   = string((char *) info->interf);

    return device;
}

inline static void delete_device(device_t device)
{
    fa_destroy(device->name);
    fa_destroy(device->host_name);
    fa_delete(device);
}

inline static stream_t new_stream(device_t device)
{
    stream_t stream         = fa_new(midi_stream);

    stream->impl            = &midi_stream_impl;
    stream->device          = device;
    stream->incoming        = fa_list_empty();
    stream->native_input    = NULL;
    stream->native_output   = NULL;

    return stream;
}

inline static void delete_stream(stream_t stream)
{
    fa_delete(stream);
}


// --------------------------------------------------------------------------------

void fa_midi_initialize()
{
    pm_mutex  = fa_thread_create_mutex();
    pm_status = false;
}

void fa_midi_terminate()
{
    fa_thread_destroy_mutex(pm_mutex);
}

// --------------------------------------------------------------------------------


session_t fa_midi_begin_session()
{
    PmError result;

    if (!pm_mutex) {
        assert(false && "Module not initalized");
    }

    inform(string("Initializing real-time midi session"));

    fa_thread_lock(pm_mutex);
    {
        if (pm_status) {
            fa_thread_unlock(pm_mutex);
            return (session_t) midi_device_error(string("Overlapping real-time midi sessions"));
        } else {
            result = Pm_Initialize();

            if (result < 0) {
                return (session_t) native_error(string("Could not start midi"), result);
            }

            pm_status = true;
            fa_thread_unlock(pm_mutex);

            session_t session = new_session();
            session_init_devices(session);
            // session->acquired = time(NULL);      // TODO
            return session;
        }
    }
}

void fa_midi_end_session(session_t session)
{
    PmError result;

    if (!pm_mutex) {
        assert(false && "Not initalized");
    }

    inform(string("Terminating real-time midi session"));

    fa_thread_lock(pm_mutex);
    {
        if (pm_status) {
            result = Pm_Terminate();

            if (result < 0) {
                fa_error_log(NULL, native_error(string("Could not stop midi"), result));
                return;
            }

            pm_status = false;
        }
    }
    fa_thread_unlock(pm_mutex);
    delete_session(session);
}

void fa_midi_with_session(session_callback_t    session_callback,
                          fa_ptr_t         session_data,
                          error_callback_t      error_callback,
                          fa_ptr_t         error_data)
{
    session_t session = fa_midi_begin_session();

    if (fa_check(session)) {
        error_callback(error_data, (error_t) session);
    } else {
        session_callback(session_data, session);
    }

    fa_midi_end_session(session);
}

fa_list_t fa_midi_all(session_t session)
{
    return fa_copy(session->devices);
}

fa_pair_t fa_midi_default(session_t session)
{
    return pair(session->def_input, session->def_output);
}

device_t fa_midi_default_input(session_t session)
{
    return session->def_input;
}

device_t fa_midi_default_output(session_t session)
{
    return session->def_output;
}

void add_midi_status_listener(status_callback_t function, ptr_t data);

void fa_midi_set_status_callback(
    status_callback_t function,
    ptr_t             data,
    session_t         session)
{
    assert(session && "Not a real session");

    // See device_osx.c and device_win.c
    add_midi_status_listener(function, data);
}

fa_string_t fa_midi_name(device_t device)
{
    return fa_copy(device->name);
}

fa_string_t fa_midi_host_name(device_t device)
{
    return fa_copy(device->host_name);
}

bool fa_midi_has_input(device_t device)
{
    return device->input;
}

bool fa_midi_has_output(device_t device)
{
    return device->output;
}



void midi_inform_opening(device_t device)
{
    inform(string("Opening real-time midi stream"));

    if (device->input) {
        inform(string_dappend(string("    Input:  "), fa_string_show(device)));
    }

    if (device->output) {
        inform(string_dappend(string("    Output:  "), fa_string_show(device)));
    }
}

PmTimestamp midi_time_callback(void *data)
{
    return 0; // FIXME
}

fa_midi_stream_t fa_midi_open_stream(device_t device)
{
    assert(device && "Not a device");
    midi_inform_opening(device);

    PmError result;
    stream_t stream = new_stream(device);

    if (device->input) {
        inform(string("Opening input\n"));
        result = Pm_OpenInput(&stream->native_input, device->index, NULL, 0,
                              midi_time_callback, NULL);

        if (result < 0) {
            native_error(string("Could not open midi input"), result);
        }
    }

    if (device->output) {
        inform(string("Opening output\n"));
        result = Pm_OpenOutput(&stream->native_output, device->index, NULL, 0,
                               midi_time_callback, NULL, -1);

        if (result < 0) {
            native_error(string("Could not open midi output"), result);
        }
    }

    return stream;
}


void fa_midi_close_stream(stream_t stream)
{
    inform(string("Closing real-time midi stream"));

    if (stream->native_input) {
        Pm_Close(stream->native_input);
    }

    if (stream->native_output) {
        Pm_Close(stream->native_output);
    }
}


void fa_midi_with_stream(device_t           device,
                         stream_callback_t  stream_callback,
                         fa_ptr_t      stream_data,
                         error_callback_t   error_callback,
                         fa_ptr_t      error_data)
{
    stream_t stream = fa_midi_open_stream(device);

    if (fa_check(stream)) {
        error_callback(error_data, (error_t) stream);
    } else {
        stream_callback(stream_data, stream);
    }

    fa_midi_close_stream(stream);
}







// --------------------------------------------------------------------------------

fa_string_t midi_session_show(ptr_t a)
{
    string_t str = string("<MidiSession ");
    str = string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void midi_session_destroy(ptr_t a)
{
    fa_midi_end_session(a);
}

ptr_t midi_session_impl(fa_id_t interface)
{
    static fa_string_show_t midi_session_show_impl
        = { midi_session_show };
    static fa_destroy_t midi_session_destroy_impl
        = { midi_session_destroy };

    switch (interface) {
    case fa_string_show_i:
        return &midi_session_show_impl;

    case fa_destroy_i:
        return &midi_session_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool midi_device_equal(ptr_t a, ptr_t b)
{
    device_t device1 = (device_t) a;
    device_t device2 = (device_t) b;
    // // TODO check that session is valid
    return device1->index == device2->index;
}

fa_string_t midi_device_show(ptr_t a)
{
    device_t device = (device_t) a;

    string_t str = string("<MidiDevice ");
    str = string_dappend(str, fa_midi_host_name(device));
    str = string_dappend(str, string(" "));
    str = string_dappend(str, fa_midi_name(device));
    str = string_dappend(str, string(">"));
    return str;
}

ptr_t midi_device_impl(fa_id_t interface)
{
    static fa_equal_t midi_device_equal_impl
        = { midi_device_equal };
    static fa_string_show_t midi_device_show_impl
        = { midi_device_show };

    switch (interface) {
    case fa_equal_i:
        return &midi_device_equal_impl;

    case fa_string_show_i:
        return &midi_device_show_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

fa_string_t midi_stream_show(ptr_t a)
{
    string_t str = string("<MidiStream ");
    str = string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void midi_stream_destroy(ptr_t a)
{
    fa_midi_close_stream(a);
}

// void midi_stream_sync(ptr_t a)
// {
//     stream_t stream = (stream_t) a;
//
//     fa_destroy(stream->incoming);
//     stream->incoming = fa_list_empty();
//
//     // TODO need to get error?
//     while (Pm_Poll(stream->native_input) == TRUE) {
//
//         PmEvent buffer[1024];
//         PmError result = Pm_Read(stream->native_input, buffer, 1024);
//
//         if (result < 0) {
//             native_error(string("Could not receive midi"), result);
//         }
//
//         for (int i = 0; i < result; ++i) {
//             PmEvent   event = buffer[i];
//             PmMessage msg   = event.message;
//
//             // FIXME detect sysex
//             midi_message_t midi_message = midi_message(Pm_MessageStatus(msg), Pm_MessageData1(msg), Pm_MessageData2(msg));
//             stream->incoming = fa_list_dcons(midi_message, stream->incoming);
//
//             // TODO midi thru
//         }
//     }
//
// }
//
// fa_list_t midi_stream_receive(ptr_t a, address_t addr)
// {
//     // Ignore address
//     stream_t stream = (stream_t) a;
//     return fa_list_copy(stream->incoming);
// }
//
// void midi_stream_send(ptr_t a, address_t addr, message_t msg)
// {
//     PmError result;
//     stream_t stream = (stream_t) a;
//     midi_message_t midi   = (midi_message_t) msg;
//     // TODO use dynamic introspection to detect lists (?)
//
//     if (fa_midi_message_is_simple(midi)) {
//         // timestamp ignored
//         long midi_message = fa_midi_message_simple_to_long(midi);
//
//         // printf("Sending: %s %08x\n", unstring(fa_string_show(midi)), (int) midi_message);
//
//         result = Pm_WriteShort(stream->native_output, 0, midi_message);
//
//         if (result != pmNoError) {
//             native_error(string("Could not send midi"), result);
//         }
//     } else {
//         assert(false && "Not implemented");
//
//         unsigned char buf[2048];
//         buf[0]    = 'f';
//         buf[0]    = '0';
//         buf[2046] = 'f';
//         buf[2047] = '7';
//
//         // check buffer size <= (2048-2)
//         // copy sysex buffer to buf+1
//
//         result = Pm_WriteSysEx(stream->native_output, 0, buf);
//         if (result != pmNoError) {
//             native_error(string("Could not send midi"), result);
//         }
//     }
//
// }

ptr_t midi_stream_impl(fa_id_t interface)
{
    static fa_string_show_t midi_stream_show_impl
        = { midi_stream_show };
    static fa_destroy_t midi_stream_destroy_impl
        = { midi_stream_destroy };
    // static fa_message_receiver_interface_t midi_stream_message_receiver_interface_impl
    // = { midi_stream_send };
    // static fa_message_sender_interface_t midi_stream_message_sender_interface_impl
    // = { midi_stream_sync, midi_stream_receive };

    switch (interface) {


    case fa_string_show_i:
        return &midi_stream_show_impl;

    case fa_destroy_i:
        return &midi_stream_destroy_impl;

        // case fa_message_sender_interface_i:
        // return &midi_stream_message_sender_interface_impl;

        // case fa_message_receiver_interface_i:
        // return &midi_stream_message_receiver_interface_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

void fa_fa_log_error_from(fa_string_t msg, fa_string_t origin);

error_t midi_device_error(string_t msg)
{
    return fa_error_create_simple(error,
                                  msg,
                                  string("Doremir.Device.Midi"));
}

error_t midi_device_error_with(string_t msg, int code)
{
    return fa_error_create_simple(error,
                                  string_dappend(msg, format_integral(" (error code %d)", code)),
                                  string("Doremir.Device.Midi"));
}
error_t native_error(string_t msg, int code)
{
    return fa_error_create_simple(error,
                                  string_dappend(msg, string((char *) Pm_GetErrorText(code))),
                                  string("Doremir.Device.Midi"));
}


void midi_device_fatal(string_t msg, int code)
{
    fa_fa_log_error_from(
        string_dappend(msg, format_integral(" (error code %d)", code)),
        string("Doremir.Device.Midi"));

    fa_fa_log_error(string("Terminating Fa"));
    exit(error);
}

