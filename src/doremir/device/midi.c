
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/device/midi.h>
#include <doremir/midi.h>
#include <doremir/list.h>
#include <doremir/thread.h>
#include <doremir/util.h>

#include <portmidi.h>

/*
    Notes:
        * Device detection handled by excplicit CoreMIDI/?? calls
        * Call MIDIRestart() ?
        * TODO proper error checking
 */

typedef doremir_device_midi_t                  device_t;
typedef doremir_device_midi_stream_t           stream_t;
typedef doremir_device_midi_session_t          session_t;
typedef doremir_device_midi_stream_callback_t  stream_callback_t;
typedef doremir_device_midi_session_callback_t session_callback_t;
typedef doremir_device_midi_status_callback_t  status_callback_t;

typedef PmDeviceID    native_index_t;
typedef PmStream     *native_stream_t;

struct _doremir_device_midi_session_t {

    impl_t              impl;               // Dispatcher
    system_time_t       acquired;           // Time of acquisition (not used at the moment)

    list_t              devices;            // Cached device list

    device_t            def_input;          // Default devices, both possibly null
    device_t            def_output;         // If present, these are also in the above list
};

struct _doremir_device_midi_t {

    impl_t              impl;               // Dispatcher
    native_index_t      index;              // Native device index

    bool                input, output;      // Cached capabilities
    string_t            name;               // Cached names
    string_t            host_name;
};

struct _doremir_device_midi_stream_t {

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
ptr_t midi_session_impl(doremir_id_t interface);
ptr_t midi_device_impl(doremir_id_t interface);
ptr_t midi_stream_impl(doremir_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(native_index_t index);
inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t device);
inline static void delete_stream(stream_t stream);

long doremir_midi_simple_to_long(doremir_midi_t midi);


// --------------------------------------------------------------------------------

inline static session_t new_session()
{
    session_t session = doremir_new(device_midi_session);
    session->impl = &midi_session_impl;
    return session;
}

inline static void session_init_devices(session_t session)
{
    native_index_t count;
    list_t         devices;

    count   = Pm_CountDevices();
    devices = doremir_list_empty();

    for (size_t i = 0; i < count; ++i) {
        device_t device = new_device(i);

        if (device) {
            devices = doremir_list_dcons(device, devices);
        }
    }

    session->devices      = doremir_list_dreverse(devices);
    session->def_input    = new_device(Pm_GetDefaultInputDeviceID());
    session->def_output   = new_device(Pm_GetDefaultOutputDeviceID());
}

inline static void delete_session(session_t session)
{
    // TODO free device list
    doremir_delete(session);
}

inline static device_t new_device(native_index_t index)
{
    if (index == pmNoDevice) {
        return NULL;
    }

    device_t device = doremir_new(device_midi);
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
    doremir_destroy(device->name);
    doremir_destroy(device->host_name);
    doremir_delete(device);
}

inline static stream_t new_stream(device_t device)
{
    stream_t stream         = doremir_new(device_midi_stream);

    stream->impl            = &midi_stream_impl;
    stream->device          = device;
    stream->incoming        = doremir_list_empty();
    stream->native_input    = NULL;
    stream->native_output   = NULL;

    return stream;
}

inline static void delete_stream(stream_t stream)
{
    doremir_delete(stream);
}


// --------------------------------------------------------------------------------

void doremir_device_midi_initialize()
{
    pm_mutex  = doremir_thread_create_mutex();
    pm_status = false;
}

void doremir_device_midi_terminate()
{
    doremir_thread_destroy_mutex(pm_mutex);
}

// --------------------------------------------------------------------------------


session_t doremir_device_midi_begin_session()
{
    PmError result;
    
    if (!pm_mutex) {
        assert(false && "Module not initalized");
    }

    inform(string("Initializing real-time midi session"));

    doremir_thread_lock(pm_mutex);
    {
        if (pm_status) {
            doremir_thread_unlock(pm_mutex);
            return (session_t) midi_device_error(string("Overlapping real-time midi sessions"));
        } else {
            result = Pm_Initialize();
            if (result < 0) {
                return (session_t) native_error(string("Could not start midi"), result);
            }

            pm_status = true;
            doremir_thread_unlock(pm_mutex);

            session_t session = new_session();
            session_init_devices(session);
            // session->acquired = time(NULL);      // TODO
            return session;
        }
    }
}

void doremir_device_midi_end_session(session_t session)
{ 
    PmError result;
    
    if (!pm_mutex) {
        assert(false && "Not initalized");
    }

    inform(string("Terminating real-time midi session"));

    doremir_thread_lock(pm_mutex);
    {
        if (pm_status) {
            result = Pm_Terminate();
            if (result < 0) {
                doremir_error_log(NULL, native_error(string("Could not stop midi"), result));
                return;
            }
            pm_status = false;
        }
    }
    doremir_thread_unlock(pm_mutex);
    delete_session(session);
}

void doremir_device_midi_with_session(session_callback_t    session_callback,
                                      doremir_ptr_t         session_data,
                                      error_callback_t      error_callback,
                                      doremir_ptr_t         error_data)
{
    session_t session = doremir_device_midi_begin_session();

    if (doremir_check(session)) {
        error_callback(error_data, (error_t) session);
    } else {
        session_callback(session_data, session);
    }

    doremir_device_midi_end_session(session);
}

doremir_list_t doremir_device_midi_all(session_t session)
{
    return doremir_copy(session->devices);
}

doremir_pair_t doremir_device_midi_default(session_t session)
{
    return pair(session->def_input, session->def_output);
}

device_t doremir_device_midi_default_input(session_t session)
{
    return session->def_input;
}

device_t doremir_device_midi_default_output(session_t session)
{
    return session->def_output;
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
    return doremir_copy(device->name);
}

doremir_string_t doremir_device_midi_host_name(device_t device)
{
    return doremir_copy(device->host_name);
}

bool doremir_device_midi_has_input(device_t device)
{
    return device->input;
}

bool doremir_device_midi_has_output(device_t device)
{
    return device->output;
}



void midi_inform_opening(device_t device)
{
    inform(string("Opening real-time midi stream"));

    if (device->input) {
        inform(string_dappend(string("    Input:  "), doremir_string_show(device)));
    }

    if (device->output) {
        inform(string_dappend(string("    Output:  "), doremir_string_show(device)));
    }
}

PmTimestamp midi_time_callback(void *data)
{
    return 0; // FIXME
}

doremir_device_midi_stream_t doremir_device_midi_open_stream(device_t device)
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


void doremir_device_midi_close_stream(stream_t stream)
{
    inform(string("Closing real-time midi stream"));
    if (stream->native_input)
        Pm_Close(stream->native_input);
    if (stream->native_output)
        Pm_Close(stream->native_output);
}


void doremir_device_midi_with_stream(device_t           device,
                                     stream_callback_t  stream_callback,
                                     doremir_ptr_t      stream_data,
                                     error_callback_t   error_callback,
                                     doremir_ptr_t      error_data)
{
    stream_t stream = doremir_device_midi_open_stream(device);

    if (doremir_check(stream)) {
        error_callback(error_data, (error_t) stream);
    } else {
        stream_callback(stream_data, stream);
    }

    doremir_device_midi_close_stream(stream);
}







// --------------------------------------------------------------------------------

doremir_string_t midi_session_show(ptr_t a)
{
    string_t str = string("<MidiSession ");
    str = string_dappend(str, doremir_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void midi_session_destroy(ptr_t a)
{
    doremir_device_midi_end_session(a);
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
    device_t device1 = (device_t) a;
    device_t device2 = (device_t) b;
    // // TODO check that session is valid
    return device1->index == device2->index;
}

doremir_string_t midi_device_show(ptr_t a)
{
    device_t device = (device_t) a;

    string_t str = string("<MidiDevice ");
    str = string_dappend(str, doremir_device_midi_host_name(device));
    str = string_dappend(str, string(" "));
    str = string_dappend(str, doremir_device_midi_name(device));
    str = string_dappend(str, string(">"));
    return str;
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
    string_t str = string("<MidiStream ");
    str = string_dappend(str, doremir_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void midi_stream_destroy(ptr_t a)
{
    doremir_device_midi_close_stream(a);
}

void midi_stream_sync(ptr_t a)
{
    stream_t stream = (stream_t) a;

    doremir_destroy(stream->incoming);
    stream->incoming = doremir_list_empty();

    // TODO need to get error?
    while (Pm_Poll(stream->native_input) == TRUE) {

        PmEvent buffer[1024];
        PmError result = Pm_Read(stream->native_input, buffer, 1024);
        
        if (result < 0) {
            native_error(string("Could not receive midi"), result);
        }
        
        for (int i = 0; i < result; ++i) {
            PmEvent   event = buffer[i];
            PmMessage msg   = event.message;

            // FIXME detect sysex
            midi_t midi = midi(Pm_MessageStatus(msg), Pm_MessageData1(msg), Pm_MessageData2(msg));
            stream->incoming = doremir_list_dcons(midi, stream->incoming);

            // TODO midi thru
        }
    }

}

doremir_list_t midi_stream_receive(ptr_t a, address_t addr)
{
    // Ignore address
    stream_t stream = (stream_t) a;
    return doremir_list_copy(stream->incoming);
}

void midi_stream_send(ptr_t a, address_t addr, message_t msg)
{
    PmError result;
    stream_t stream = (stream_t) a;
    midi_t   midi   = (midi_t) msg;
    // TODO use dynamic introspection to detect lists (?)

    if (doremir_midi_is_simple(midi)) {
        // timestamp ignored
        long midi_msg = doremir_midi_simple_to_long(midi);

        // printf("Sending: %s %08x\n", unstring(doremir_string_show(midi)), (int) midi_msg);

        result = Pm_WriteShort(stream->native_output, 0, midi_msg);

        if (result != pmNoError) {
            native_error(string("Could not send midi"), result);
        }
    } else {
        assert(false && "Not implemented");

        unsigned char buf[2048];
        buf[0]    = 'f';
        buf[0]    = '0';
        buf[2046] = 'f';
        buf[2047] = '7';

        // check buffer size <= (2048-2)
        // copy sysex buffer to buf+1

        result = Pm_WriteSysEx(stream->native_output, 0, buf);
        if (result != pmNoError) {
            native_error(string("Could not send midi"), result);
        }
    }

}

ptr_t midi_stream_impl(doremir_id_t interface)
{
    static doremir_string_show_t midi_stream_show_impl
        = { midi_stream_show };
    static doremir_destroy_t midi_stream_destroy_impl
        = { midi_stream_destroy };
    static doremir_message_receiver_interface_t midi_stream_message_receiver_interface_impl
        = { midi_stream_send };
    static doremir_message_sender_interface_t midi_stream_message_sender_interface_impl
        = { midi_stream_sync, midi_stream_receive };

    switch (interface) {


    case doremir_string_show_i:
        return &midi_stream_show_impl;

    case doremir_destroy_i:
        return &midi_stream_destroy_impl;

    case doremir_message_sender_interface_i:
        return &midi_stream_message_sender_interface_impl;

    case doremir_message_receiver_interface_i:
        return &midi_stream_message_receiver_interface_impl;

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
                                       string("Doremir.Device.Midi"));
}

error_t midi_device_error_with(string_t msg, int code)
{
    return doremir_error_create_simple(error,
                                       string_dappend(msg, format_integral(" (error code %d)", code)),
                                       string("Doremir.Device.Midi"));
}     
error_t native_error(string_t msg, int code)
{
    return doremir_error_create_simple(error,
                                       string_dappend(msg, string((char*) Pm_GetErrorText(code))),
                                       string("Doremir.Device.Midi"));
}     


void midi_device_fatal(string_t msg, int code)
{
    doremir_audio_engine_log_error_from(
        string_dappend(msg, format_integral(" (error code %d)", code)),
        string("Doremir.Device.Midi"));

    doremir_audio_engine_log_error(string("Terminating Audio Engine"));
    exit(error);
}

