
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/atomic.h>
#include <fa/midi.h>
#include <fa/midi/message.h>
#include <fa/atomic/queue.h>
#include <fa/pair/left.h>
#include <fa/priority_queue.h>
#include <fa/list.h>
#include <fa/thread.h>
#include <fa/time.h>
#include <fa/clock.h>
#include <fa/util.h>

#include <portmidi.h>

/*
    ## Notes

 */

typedef fa_midi_device_t            device_t;
typedef fa_midi_stream_t            stream_t;
typedef fa_midi_session_t           session_t;
typedef fa_midi_stream_callback_t   stream_callback_t;
typedef fa_midi_session_callback_t  session_callback_t;
typedef fa_midi_status_callback_t   status_callback_t;
typedef fa_action_t                 action_t;

typedef PmDeviceID                  native_index_t;
typedef PmStream                   *native_stream_t;

#define kMaxMessageCallbacks       8
#define kMidiServiceThreadInterval 20

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
                        native_output;      // Native stream(s)
    device_t            device;

    thread_t            thread;
    bool                thread_abort;       // Set to non-zero when thread should stop

    // fa_atomic_t         receivers;          // Atomic [(Unary, Ptr)]
    int                 message_callback_count;
    fa_unary_t          message_callbacks[kMaxMessageCallbacks];
    fa_ptr_t            message_callback_ptrs[kMaxMessageCallbacks];

    fa_clock_t          clock;              // Clock used for scheduler and incoming events
    atomic_queue_t      in_controls;        // Controls for scheduling, (AtomicQueue (Time, (Channel, Ptr)))
    priority_queue_t    controls;           // Scheduled controls (Time, (Channel, Ptr))

    // list_t              incoming;
};

static mutex_t   pm_mutex;
static bool      pm_status;
static session_t midi_current_session;

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

    stream->native_input    = NULL;
    stream->native_output   = NULL;

    stream->thread          = NULL;
    stream->thread_abort    = false;
    stream->message_callback_count = 0;
    
    stream->clock           = fa_clock_standard(); // TODO change
    stream->in_controls     = atomic_queue();
    stream->controls        = priority_queue();

    return stream;
}

inline static void delete_stream(stream_t stream)
{
    fa_delete(stream);
}


// --------------------------------------------------------------------------------

void fa_midi_initialize()
{
    pm_mutex        = fa_thread_create_mutex();
    pm_status       = false;
    midi_current_session = NULL;
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

            midi_current_session = session;
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
            inform(string("(actually terminating)"));
            result = Pm_Terminate();

            if (result < 0) {
                fa_error_log(NULL, native_error(string("Could not stop midi"), result));
                return;
            }

            pm_status = false;
            midi_current_session = NULL;
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

fa_list_t fa_midi_current_sessions()
{
    if (!midi_current_session) {
        return list();
    } else {
        return list(midi_current_session);
    }
}

fa_ptr_t fa_midi_end_all_sessions()
{
    fa_dfor_each(x, fa_midi_current_sessions()) {
        fa_midi_end_session(x);
    }
    return NULL;
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

void fa_midi_add_status_callback(status_callback_t function,
                                 ptr_t             data,
                                 session_t         session)
{
    assert(session && "Not a real session");

    // See platform/*/device.c
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







static inline
void send_out(midi_message_t midi, stream_t stream);

PmTimestamp midi_time_callback(void *data)
{
    stream_t stream = data;
    return fa_clock_milliseconds(stream->clock); // FIXME
}

ptr_t stream_thread_callback(ptr_t x);

fa_midi_stream_t fa_midi_open_stream(device_t device)
{
    if (!device) {
        return (stream_t) midi_device_error_with(
            string("Can not open a stream with no devices"), 0);
    }

    midi_inform_opening(device);

    PmError result;
    stream_t stream = new_stream(device);

    if (device->input) {
        inform(string("Opening input\n"));
        result = Pm_OpenInput(&stream->native_input, device->index, NULL, 0,
                              midi_time_callback, stream);

        if (result < 0) {
            native_error(string("Could not open midi input"), result);
        }
    }

    if (device->output) {
        inform(string("Opening output\n"));
        result = Pm_OpenOutput(&stream->native_output, device->index, NULL, 0,
                               midi_time_callback, stream, -1);

        if (result < 0) {
            native_error(string("Could not open midi output"), result);
        }
    }
    
    stream->thread = fa_thread_create(stream_thread_callback, stream);
    return stream;
}


void fa_midi_close_stream(stream_t stream)
{
    inform(string("Closing real-time midi stream"));

    // TODO instruct to stop
    stream->thread_abort = true;
    fa_thread_join(stream->thread);

    if (stream->native_input) {
        Pm_Close(stream->native_input);
    }

    if (stream->native_output) {
        Pm_Close(stream->native_output);
    }
}


void fa_midi_with_stream(device_t           device,
                         stream_callback_t  stream_callback,
                         fa_ptr_t           stream_data,
                         error_callback_t   error_callback,
                         fa_ptr_t           error_data)
{
    stream_t stream = fa_midi_open_stream(device);

    if (fa_check(stream)) {
        error_callback(error_data, (error_t) stream);
    } else {
        stream_callback(stream_data, stream);
    }

    fa_midi_close_stream(stream);
}




void read_in(PmEvent* dest, stream_t stream);

ptr_t stream_thread_callback(ptr_t x)
{                 
    stream_t stream = x;
    inform(string("  Midi service thread active"));

    while(1) {              
        if (stream->thread_abort) {
            inform(string("  Midi service thread finished"));
            return 0;
        }
        // inform(string("  Midi service thread!"));

        // Inputs
        if (stream->native_input) {
            while (Pm_Poll(stream->native_input)) {

                PmEvent events[1];
                read_in(events, stream);
                // Pm_Read(stream->native_input, events, 1); // TODO error
                
                for (int i = 0; i < stream->message_callback_count; ++i) {
                    unary_t f = stream->message_callbacks[i];
                    ptr_t   x = stream->message_callback_ptrs[i];
                    
                    time_t time = fa_milliseconds(events[0].timestamp);
                    midi_message_t msg = midi_message(
                        Pm_MessageStatus(events[0].message), 
                        Pm_MessageData1(events[0].message), 
                        Pm_MessageData2(events[0].message));

                    f(x, pair(time, msg));
                }
            }
        }

        // Outputs
        if (stream->native_output) {
            ptr_t val;
            while ((val = fa_atomic_queue_read(stream->in_controls))) {
                // inform(fa_string_show(val));
                fa_priority_queue_insert(fa_pair_left_from_pair(val), stream->controls);
            }
            while (1) {
                pair_t x = fa_priority_queue_peek(stream->controls);
                if (!x) {
                    break;
                }
                time_t   time   = fa_pair_first(x);
                action_t action = fa_pair_second(x);

                int timeSamp = (((double) fa_time_to_milliseconds(time)) / 1000.0) * 44100;   // TODO

                if (timeSamp <= fa_clock_milliseconds(stream->clock)) {
                    if (fa_action_is_send(action)) {
                        string_t name = fa_action_send_name(action);
                        ptr_t    value = fa_action_send_value(action);
                        send_out(value, stream); // TODO laterz
                        mark_used(name);
                    }
                    fa_priority_queue_pop(stream->controls);
                } else {
                    break;
                }
            }
        }
        
        // Sleep
        fa_thread_sleep(kMidiServiceThreadInterval);
    }
    assert(false && "Unreachable");
}

void fa_midi_add_message_callback(fa_midi_message_callback_t function,
                                  fa_ptr_t data,
                                  fa_midi_stream_t stream)
{
    assert (stream->message_callback_count < kMaxMessageCallbacks && "Too many message callbacks");

    int i = stream->message_callback_count;
    stream->message_callbacks[i] = function;
    stream->message_callback_ptrs[i] = data;
    
    stream->message_callback_count++;
}



void fa_midi_schedule(fa_time_t        time,
                      fa_action_t      action,
                      fa_midi_stream_t stream)
{
    pair_left_t pair = pair_left(time, action);
    fa_atomic_queue_write(stream->in_controls, pair);


    // if (fa_action_is_send(action)) {
    //     string_t name = fa_action_send_name(action);
    //     ptr_t    value = fa_action_send_value(action);
    //     send_out(value, stream); // TODO laterz
    //     mark_used(name);
    // }
}

            
void read_in(PmEvent* dest, stream_t stream)
{   
    PmError result;
    result = Pm_Read(stream->native_input, dest, 1);

    if (result != pmNoError) {
        native_error(string("Could not fetch midi"), result);
    }
}

void send_out(midi_message_t midi, stream_t stream)
{
    PmError result;

    if (fa_midi_message_is_simple(midi)) {
        // timestamp ignored
        long midi_message = fa_midi_message_simple_to_long(midi);

        printf("Sending: %s %08x\n", unstring(fa_string_show(midi)), (int) midi_message);

        result = Pm_WriteShort(stream->native_output, 0, midi_message);

        if (result != pmNoError) {
            native_error(string("Could not send midi"), result);
        }
    } else {
        assert(false && "Can not send sysex yet");
    }
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
    // TODO check that session is valid
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

ptr_t midi_stream_impl(fa_id_t interface)
{
    static fa_string_show_t midi_stream_show_impl
        = { midi_stream_show };
    static fa_destroy_t midi_stream_destroy_impl
        = { midi_stream_destroy };

    switch (interface) {


    case fa_string_show_i:
        return &midi_stream_show_impl;

    case fa_destroy_i:
        return &midi_stream_destroy_impl;

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

