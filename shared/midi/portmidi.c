
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
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
#include <fa/dynamic.h>
#include <fa/action.h>

#include <portmidi.h>

#include "../shared/action_internal.h"

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

#define kMaxMessageCallbacks       64
#define kMidiServiceThreadInterval 20

struct _fa_midi_session_t {

    fa_impl_t           impl;               // Dispatcher
    system_time_t       acquired;           // Time of acquisition (not used at the moment)

    fa_list_t           devices;            // Cached device list
    fa_list_t           streams;            // All streams started on this sessiuon (list of stream_t)

    device_t            def_input;          // Default devices, both possibly null
    device_t            def_output;         // If present, these are also in the above list
};

struct _fa_midi_device_t {

    fa_impl_t           impl;               // Dispatcher
    native_index_t      index;              // Native device index
    session_t           session;            // Underlying session

    bool                input, output;      // Cached capabilities
    fa_string_t         name;               // Cached names
    fa_string_t         host_name;
};

struct _fa_midi_stream_t {

    fa_impl_t           impl;               // Dispatcher
    native_stream_t     native_input,
                        native_output;      // Native stream(s)
    device_t            device;

    fa_thread_t         thread;
    bool                aborted;            // Set to non-zero when thread should stop (and native streams stopped)

    // fa_atomic_t      receivers;          // Atomic [(Unary, Ptr)]
    int                 message_callback_count;
    fa_unary_t          message_callbacks[kMaxMessageCallbacks];
    fa_ptr_t            message_callback_ptrs[kMaxMessageCallbacks];

    fa_clock_t          clock;              // Clock used for scheduler and incoming events
    fa_atomic_queue_t   in_controls;        // Controls for scheduling, (AtomicQueue (Time, (Channel, Ptr)))
    fa_atomic_queue_t   short_controls;     // Controls to run directly (optimization)
    fa_priority_queue_t controls;           // Scheduled controls (Time, (Channel, Ptr))

    // fa_list_t           incoming;
};

static fa_thread_mutex_t   pm_mutex;
static bool      pm_status;
static session_t midi_current_session;

fa_error_t midi_device_error(fa_string_t msg);
fa_error_t midi_device_error_with(fa_string_t msg, int error);
fa_error_t native_error(fa_string_t msg, int code);
void midi_device_fatal(fa_string_t msg, int code);
fa_ptr_t midi_session_impl(fa_id_t interface);
fa_ptr_t midi_device_impl(fa_id_t interface);
fa_ptr_t midi_stream_impl(fa_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(native_index_t index, session_t session);
inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t device);
inline static void delete_stream(stream_t stream);

long fa_midi_message_simple_to_long(fa_midi_message_t midi);


// --------------------------------------------------------------------------------

inline static session_t new_session()
{
    session_t session = fa_new(midi_session);
    session->impl = &midi_session_impl;
    session->streams = fa_empty();
    return session;
}

inline static void session_init_devices(session_t session)
{
    native_index_t count;
    fa_list_t         devices;
    device_t       input = NULL, output = NULL;

    count   = Pm_CountDevices();
    devices = fa_empty();

    for (size_t i = 0; i < count; ++i) {
        device_t device = new_device(i, session);

        if (device) {
            devices = fa_list_dcons(device, devices);
        }

        if (i == Pm_GetDefaultInputDeviceID()) {
            input = device;
        }

        if (i == Pm_GetDefaultOutputDeviceID()) {
            output = device;
        }
    }

    session->devices      = fa_list_dreverse(devices);
    session->def_input    = input;
    session->def_output   = output;
}

inline static void delete_session(session_t session)
{
    // TODO free device list
    fa_delete(session);
}

inline static device_t new_device(native_index_t index, session_t session)
{
    if (index == pmNoDevice) {
        return NULL;
    }

    device_t device = fa_new(midi_device);
    device->impl    = &midi_device_impl;
    device->session = session;

    const PmDeviceInfo  *info = Pm_GetDeviceInfo(index);

    device->index       = index;
    device->input       = info->input;
    device->output      = info->output;
#ifndef _WIN32
    device->name        = fa_string_from_utf8((char *) info->name);                      // const cast
    device->host_name   = fa_string_from_utf8((char *) info->interf);
#else
    device->name        = fa_string_from_cp1252((char *) info->name);      // const cast
    device->host_name   = fa_string_from_cp1252((char *) info->interf);
#endif

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
    stream->aborted         = false;
    stream->message_callback_count = 0;

    stream->clock           = fa_clock_standard(); // TODO change
    stream->in_controls     = fa_atomic_queue();
    stream->short_controls  = fa_atomic_queue();
    stream->controls        = fa_priority_queue();

    return stream;
}

inline static void delete_stream(stream_t stream)
{
    // TODO flush pending events
    fa_destroy(stream->controls);
    fa_destroy(stream->in_controls);
    fa_destroy(stream->short_controls);
    fa_delete(stream);
}


// --------------------------------------------------------------------------------

void fa_midi_initialize()
{
    pm_mutex             = fa_thread_create_mutex();
    pm_status            = false;
    midi_current_session = NULL;

    fa_inform(fa_string("    Using PortMIDI as MIDI backend."));
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

    fa_inform(fa_string("Initializing real-time midi session"));

    session_t session;

    fa_with_lock(pm_mutex) {
        if (pm_status) {
            session = (session_t) midi_device_error(fa_string("Overlapping real-time midi sessions"));
        } else {
            result = Pm_Initialize();

            if (result < 0) {
                session = (session_t) native_error(fa_string("Could not start midi"), result);
            } else {
                pm_status = true;

                session = new_session();
                session_init_devices(session);

                midi_current_session = session;
            }
        }
    }
    return session;
}

void fa_midi_end_session(session_t session)
{
    PmError result;

    if (!pm_mutex) {
        assert(false && "Not initalized");
    }

    fa_inform(fa_string("Terminating real-time midi session"));

    fa_with_lock(pm_mutex) {
        if (pm_status) {
            fa_inform(fa_string("(actually terminating)"));

            fa_inform(fa_string("(closing streams)"));
            fa_for_each(stream, session->streams) {
                // It is OK if the stream is already closed
                fa_midi_close_stream(stream);
                delete_stream(stream);
            }
            fa_inform(fa_string("(stopping driver)"));

            result = Pm_Terminate();
            fa_thread_sleep(300); // TODO needed?

            if (result < 0) {
                fa_error_log(NULL, native_error(fa_string("Could not stop midi"), result));
            } else {
                pm_status = false;
                midi_current_session = NULL;
                delete_session(session);
            }
        }
    }
}

void fa_midi_with_session(session_callback_t    session_callback,
                          fa_ptr_t         session_data,
                          fa_error_callback_t      error_callback,
                          fa_ptr_t         error_data)
{
    session_t session = fa_midi_begin_session();

    if (fa_check(session)) {
        error_callback(error_data, (fa_error_t) session);
    } else {
        session_callback(session_data, session);
    }

    fa_midi_end_session(session);
}

fa_list_t fa_midi_current_sessions()
{
    if (!midi_current_session) {
        return fa_empty();
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

#define fail_if_no_input(type) \
    if (!session->def_input) { \
        return (type) midi_device_error(fa_string("No input device available")); \
    }
#define fail_if_no_output(type) \
    if (!session->def_output) { \
        return (type) midi_device_error(fa_string("No output device available")); \
    }

fa_pair_t fa_midi_default(session_t session)
{
    fail_if_no_input(fa_pair_t);
    fail_if_no_output(fa_pair_t);
    return fa_pair_create(session->def_input, session->def_output);
}

device_t fa_midi_default_input(session_t session)
{
    fail_if_no_input(device_t);
    return session->def_input;
}

device_t fa_midi_default_output(session_t session)
{
    fail_if_no_output(device_t);
    return session->def_output;
}

void add_midi_status_listener(status_callback_t function, fa_ptr_t data);

void fa_midi_add_status_callback(status_callback_t function,
                                 fa_ptr_t             data,
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



void midi_fa_inform_opening(device_t device)
{
    fa_inform(fa_string("Opening real-time midi stream"));

    if (device->input) {
        fa_inform(fa_string_dappend(fa_string("    Input:  "), fa_string_show(device)));
    }

    if (device->output) {
        fa_inform(fa_string_dappend(fa_string("    Output:  "), fa_string_show(device)));
    }
}







static inline void send_midi(stream_t stream, fa_midi_message_t midi);

PmTimestamp midi_time_callback(void *data)
{
    stream_t stream = data;
    return fa_clock_milliseconds(stream->clock);
}

fa_ptr_t stream_thread_callback(fa_ptr_t x);

fa_midi_stream_t fa_midi_open_stream(device_t device)
{
    if (!device) {
        return (stream_t) midi_device_error_with(
                   fa_string("Can not open a stream with no devices"), 0);
    }

    midi_fa_inform_opening(device);

    PmError result;
    stream_t stream = new_stream(device);

    if (device->input) {
        fa_inform(fa_string("  Opening input\n"));
        result = Pm_OpenInput(&stream->native_input, device->index, NULL, 0,
                              midi_time_callback, stream);

        if (result < 0) {
            native_error(fa_string("Could not open midi input"), result);
        }
    }

    if (device->output) {
        fa_inform(fa_string("  Opening output\n"));
        result = Pm_OpenOutput(&stream->native_output, device->index, NULL, 0,
                               midi_time_callback, stream, -1);

        if (result < 0) {
            native_error(fa_string("Could not open midi output"), result);
        }
    }

    stream->thread = fa_thread_create(stream_thread_callback, stream, fa_string("MIDI control thread"));

    fa_push_list(stream, device->session->streams);
    return stream;
}


void fa_midi_close_stream(stream_t stream)
{
    fa_inform(fa_string("Closing real-time midi stream"));

    if (!stream->aborted) {
        fa_inform(fa_string("  (Aborting midi service thread)"));
        stream->aborted = true;
        // Wait
        fa_thread_join(stream->thread);

        fa_inform(fa_string("  (Closing native streams)"));

        if (stream->native_input) {
            Pm_Close(stream->native_input);
        }

        if (stream->native_output) {
            Pm_Close(stream->native_output);
        }
    }

    // Not deleted until session is gone
}


void fa_midi_with_stream(device_t           device,
                         stream_callback_t  stream_callback,
                         fa_ptr_t           stream_data,
                         fa_error_callback_t   error_callback,
                         fa_ptr_t           error_data)
{
    stream_t stream = fa_midi_open_stream(device);

    if (fa_check(stream)) {
        error_callback(error_data, (fa_error_t) stream);
    } else {
        stream_callback(stream_data, stream);
    }

    fa_midi_close_stream(stream);
}




void receive_midi(stream_t stream, PmEvent *dest);
fa_ptr_t send_midi_action(fa_ptr_t stream, fa_ptr_t action, fa_ptr_t time);



fa_ptr_t stream_thread_callback(fa_ptr_t x)
{
    stream_t stream = x;
    fa_inform(fa_string("  Midi service thread active"));

    while (true) {
        // Exit if stream is stopped
        if (stream->aborted) {
            fa_inform(fa_string("  Midi service thread finished"));
            return 0;
        }

        // Inputs
        if (stream->native_input) {

            // While messages available
            while (Pm_Poll(stream->native_input)) {

                // Fetch one message
                PmEvent events[1];
                receive_midi(stream, events);

                // Invoke all callbacks
                for (int i = 0; i < stream->message_callback_count; ++i) {
                    fa_unary_t f = stream->message_callbacks[i];
                    fa_ptr_t   x = stream->message_callback_ptrs[i];

                    // This value from the stream clock, fetched by system thread
                    // We could also fetch it here, but that is probably less precise.
                    fa_time_t time = fa_milliseconds(events[0].timestamp);

                    if (Pm_MessageStatus(events[0].message) != 0xf0
                            &&
                            Pm_MessageStatus(events[0].message) != 0xf7) {
                        fa_midi_message_t msg = fa_midi_message(
                                                    Pm_MessageStatus(events[0].message),
                                                    Pm_MessageData1(events[0].message),
                                                    Pm_MessageData2(events[0].message));

                        f(x, fa_pair_create(time, msg));
                    } else {
                        fa_warn(fa_string("PortMIDI received SysEx: ignoring"));
                        // TODO handle sysex
                    }
                }
            }
        }

        // Outputs
        if (stream->native_output) {
            fa_ptr_t val;
            
            while ((val = fa_atomic_queue_read(stream->short_controls))) {
                send_midi_action(stream, val, NULL);
            }
            
            while ((val = fa_atomic_queue_read(stream->in_controls))) {
                fa_priority_queue_insert(fa_pair_left_from_pair(val), stream->controls);
            }

            fa_time_t   now    = fa_clock_time(stream->clock);
            run_actions(stream->controls,
                        now,
                        send_midi_action,
                        stream
                       );
            fa_destroy(now); // was mark_used
        }

        // Sleep
        fa_thread_sleep(kMidiServiceThreadInterval);
    }

    assert(false && "Unreachable");
}

fa_ptr_t send_midi_action(fa_ptr_t stream, fa_ptr_t action, fa_ptr_t time)
{
    if (fa_action_is_send(action)) {
        // fa_string_t name = fa_action_send_name(action);
        fa_ptr_t    value = fa_action_send_value(action);
        send_midi(stream, value);
        // fa_mark_used_name(name);
    }

    // TODO other actions

    return stream;
}


void fa_midi_add_message_callback(fa_midi_message_callback_t function,
                                  fa_ptr_t data,
                                  fa_midi_stream_t stream)
{
    assert(stream->message_callback_count < kMaxMessageCallbacks && "Too many message callbacks");

    int i = stream->message_callback_count;
    stream->message_callbacks[i] = function;
    stream->message_callback_ptrs[i] = data;

    stream->message_callback_count++;
}



void fa_midi_schedule(fa_time_t        time,
                      fa_action_t      action,
                      fa_midi_stream_t stream)
{
    fa_pair_left_t pair = fa_pair_left_create(time, action);
    fa_atomic_queue_write(stream->in_controls, pair);
}


/** Fetches one message from the given stream.
    Fails if the stream has no input.
 */
void receive_midi(stream_t stream, PmEvent *dest)
{
    assert(stream->native_input && "Sending to non-input stream");

    PmError result;
    result = Pm_Read(stream->native_input, dest, 1);

    if (result != pmNoError) {
        native_error(fa_string("Could not fetch midi"), result);
    }
}

/** Send a messages to the given stream.
    Fails if the stream has no output.
 */
void send_midi(stream_t stream, fa_midi_message_t midi)
{
    assert(stream->native_output && "Sending to non-output stream");

    PmError result;

    if (fa_midi_message_is_simple(midi)) {
        // Timestamp ignored, as we use faudio clock time instead
        long midi_message = fa_midi_message_simple_to_long(midi);

        result = Pm_WriteShort(stream->native_output, 0, midi_message);

        if (result != pmNoError) {
            native_error(fa_string("Could not send midi"), result);
        }
    } else {
        assert(false && "Can not send sysex yet");
    }
}

void fa_midi_schedule_relative(fa_time_t        time,
                               fa_action_t       action,
                               fa_midi_stream_t  stream)
{
    // if (fa_time_is_zero(time) && !fa_action_is_compound(action) && !fa_action_is_do(action)) {
    //     // Pass directly to output
    //     fa_atomic_queue_write(stream->short_controls, action);
    // } else {
        fa_time_t now = fa_clock_time(stream->clock);
        fa_midi_schedule(fa_dadd(now, time), action, stream);
    // }
}

void fa_midi_schedule_now(fa_action_t action, fa_midi_stream_t stream)
{
    if (fa_action_is_flat(action)) {
        fa_list_t actions = fa_action_flat_to_list(action);
        fa_for_each(a, actions) {
            fa_atomic_queue_write(stream->short_controls, a);
        }
        fa_destroy(actions);
    } else {
        fa_warn(fa_string("Non-flat action passed to fa_midi_schedule_now"));
    }
}

void fa_midi_set_clock(fa_midi_stream_t stream, fa_clock_t clock)
{
    stream->clock = clock;
}


// --------------------------------------------------------------------------------

fa_string_t midi_session_show(fa_ptr_t a)
{
    fa_string_t str = fa_string("<MidiSession ");
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

void midi_session_destroy(fa_ptr_t a)
{
    fa_midi_end_session(a);
}

fa_ptr_t midi_session_impl(fa_id_t interface)
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

bool midi_device_equal(fa_ptr_t a, fa_ptr_t b)
{
    device_t device1 = (device_t) a;
    device_t device2 = (device_t) b;
    // TODO check that session is valid
    return device1->index == device2->index;
}

fa_string_t midi_device_show(fa_ptr_t a)
{
    device_t device = (device_t) a;

    fa_string_t str = fa_string("<MidiDevice ");
    str = fa_string_dappend(str, fa_midi_host_name(device));
    str = fa_string_dappend(str, fa_string(" "));
    str = fa_string_dappend(str, fa_midi_name(device));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

fa_ptr_t midi_device_impl(fa_id_t interface)
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

fa_string_t midi_stream_show(fa_ptr_t a)
{
    fa_string_t str = fa_string("<MidiStream ");
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

void midi_stream_destroy(fa_ptr_t a)
{
    fa_midi_close_stream(a);
}

fa_dynamic_type_repr_t midi_stream_get_type(fa_ptr_t a)
{
    return midi_stream_type_repr;
}

fa_ptr_t midi_stream_impl(fa_id_t interface)
{
    static fa_string_show_t midi_stream_show_impl
        = { midi_stream_show };
    static fa_destroy_t midi_stream_destroy_impl
        = { midi_stream_destroy };
    static fa_dynamic_t midi_stream_dynamic_impl
        = { midi_stream_get_type };

    switch (interface) {

    case fa_string_show_i:
        return &midi_stream_show_impl;

    case fa_destroy_i:
        return &midi_stream_destroy_impl;
        
    case fa_dynamic_i:
        return &midi_stream_dynamic_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

void fa_log_error_from(fa_string_t msg, fa_string_t origin);

fa_error_t midi_device_error(fa_string_t msg)
{
    return fa_error_create_simple(error,
                                  msg,
                                  fa_string("Doremir.Device.Midi"));
}

fa_error_t midi_device_error_with(fa_string_t msg, int code)
{
    return fa_error_create_simple(error,
                                  fa_string_dappend(msg, fa_format_integral(" (error code %d)", code)),
                                  fa_string("Doremir.Device.Midi"));
}
fa_error_t native_error(fa_string_t msg, int code)
{
    return fa_error_create_simple(error,
                                  fa_string_dappend(msg, fa_string_from_utf8((char *) Pm_GetErrorText(code))),
                                  fa_string("Doremir.Device.Midi"));
}


void midi_device_fatal(fa_string_t msg, int code)
{
    fa_log_error_from(
        fa_string_dappend(msg, fa_format_integral(" (error code %d)", code)),
        fa_string("Doremir.Device.Midi"));

    fa_log_error(fa_string("Terminating Fa"));
    exit(error);
}

