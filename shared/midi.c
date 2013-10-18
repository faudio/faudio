
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

#include <CoreMIDI/CoreMIDI.h>
// #include <portmidi.h>

/*
    ## Notes
    
    * Implementation in terms of CoreMIDI

      CoreMIDI requires a CFRunLoop to work properly, usually the main thread is used. This is not
      an option here, so instead we will launch thread while the session is created. The begin_session()
      function will initalize *both* session and devices, and opening/closing streams simply creates
      a handle that can be used to register callbacks on the session thread.
      
      Syncronization (between MIDI vs user thread, user must syncronize externally):
        midi callbaks 
            reads session->streams
            reads from stream->outgoing_messages

        begin_session
        end_session
        current_sessions
        end_all_sessions
        all, default*
        name, host, has_*
        add_status_callback
            Must lock to prevent interruption from NotifyProc (?)
        open_stream, close_stream
            reads+writes session
            requires lock
        add_message_callback
            Must lock to prevent interruption from ReadProc (?)
            writes?
        schedule
            writes to stream->outgoing_messages
            XXX who reads?
    
    * Mapping: 
        - Sessions <-> MIDIClients 
            - Still requiring uniqueness
        - Devices <-> MIDIEndpoints
            - We do not provide any direct access to MIDIDevices/MIDIEnities
            - Each endpoint shows up as a *separate* device
        - Streams <-> MIDIPorts

     * The MIDI streams use MIDITimeStamp() for the clock. We might change this.

 */

typedef fa_midi_session_t           session_t;
typedef fa_midi_device_t            device_t;
typedef fa_midi_stream_t            stream_t;
typedef fa_midi_session_callback_t  session_callback_t;
typedef fa_midi_stream_callback_t   stream_callback_t;
typedef fa_midi_status_callback_t   status_callback_t;
typedef fa_midi_message_callback_t  message_callback_t;
typedef fa_action_t                 action_t;

typedef MIDIClientRef               native_session_t;
typedef MIDIEndpointRef             native_device_t;
typedef MIDIPortRef                 native_stream_t;
typedef OSStatus                    native_error_t;

#define kMaxMessageCallbacks        8
#define kMaxStatusCallbacks         8
// #define midi_lock fa_thread_lock
// #define midi_unlock fa_thread_unlock
#define midi_lock mark_used
#define midi_unlock mark_used


struct _fa_midi_session_t {
    impl_t                          impl;               // Dispatcher 
    native_session_t                native;

    list_t                          devices;            // Cached device list
    device_t                        def_input;          // Default devices, both possibly null
    device_t                        def_output;         // If present, these are also in the above list
    
    thread_t                        thread;             // Thread on which the MIDI run loop is invoked
    void*                           thread_abort;       // Ref to runloop

    struct {
        int                         count;
        struct {
            fa_nullary_t            function; 
            fa_ptr_t                data; 
        }                           elements[kMaxStatusCallbacks];
    }                               callbacks;          // Status callbacks
};

struct _fa_midi_device_t {
    impl_t                          impl;               // Dispatcher
    native_device_t                 native;             // Native device
    session_t                       session;            // Enclosing session

    bool                            input, output;      // Cached capabilities
    string_t                        name, host;         // Cached names
};

struct _fa_midi_stream_t {

    impl_t                          impl;               // Dispatcher
    native_stream_t                 native;             // Native stream
    device_t                        device;             // Enclosing session

    fa_clock_t                      clock;              // Clock used for scheduler and incoming events
    atomic_queue_t                  in_controls;        // Controls for scheduling, (AtomicQueue (Time, (Channel, Ptr)))
    priority_queue_t                controls;           // Scheduled controls (Time, (Channel, Ptr))

    struct {
        int                         count;
        struct {
            fa_unary_t              function; 
            fa_ptr_t                data; 
        }                           elements[kMaxMessageCallbacks];
    }                               callbacks;          // Message callbacks
};

/*
    We must track the active session for fa_midi_current_sessions().

    The mutex is needed to prevent races in start/stop and assure that at most one session
    is active at a time.
 */
static mutex_t                      gMidiMutex;
static bool                         gMidiActive;
static session_t                    gMidiCurrentSession;

error_t midi_device_error(string_t msg);
error_t midi_device_error_with(string_t msg, int error);
error_t midi_error(string_t msg, int code);
void midi_device_fatal(string_t msg, int code);
ptr_t midi_session_impl(fa_id_t interface);
ptr_t midi_device_impl(fa_id_t interface);
ptr_t midi_stream_impl(fa_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(bool is_output, native_device_t native, session_t session);
inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t device);
inline static void delete_stream(stream_t stream);
long fa_midi_message_simple_to_long(fa_midi_message_t midi);


// --------------------------------------------------------------------------------

/** Intializes everything except native, devices and thread.
 */
inline static session_t new_session()
{
    session_t session   = fa_new(midi_session);
    session->impl       = &midi_session_impl;
    
    // thread
    session->thread             = NULL;
    session->thread_abort       = NULL;
    session->callbacks.count    = 0;
    
    return session;
}

/** Intializes def_input, def_output and devices.
 */
inline static void session_init_devices(session_t session)
{
    list_t         devices = fa_list_empty();
    device_t       input = NULL, output = NULL;
    
    /* Add outputs first, then reverse lists (as we push at front).
     */                     
    int m = MIDIGetNumberOfDestinations();
    for (size_t i = 0; i < m; ++i) {
        native_device_t native = MIDIGetDestination(i);
        device_t device = new_device(true, native, session);
        if (device) {
            devices = fa_list_dcons(device, devices);
        }
        if (i == 0) {
            output = device;
        }
    }                       
    int n = MIDIGetNumberOfSources();
    for (size_t i = 0; i < n; ++i) {
        native_device_t native = MIDIGetSource(i);
        device_t device = new_device(false, native, session);
        if (device) {
            devices = fa_list_dcons(device, devices);
        }
        if (i == 0) {
            input = device;
        }
    }             
    session->def_input  = input;
    session->def_output = output;
    session->devices    = fa_list_dreverse(devices);
}

inline static void delete_session(session_t session)
{
    // TODO free device list
    fa_delete(session);
}

inline static device_t new_device(bool is_output, native_device_t native, session_t session)
{                                 
    device_t device     = fa_new(midi_device);
    device->impl        = &midi_device_impl;
    device->native      = native;
    device->session     = session;

    device->input       = !is_output;
    device->output      = is_output;

    {
        CFStringRef name;
        if (MIDIObjectGetStringProperty(native, kMIDIPropertyName, &name)) {
            assert(false && "Could not get name");
        }
        device->name = fa_string_from_native((void*) name);
    }
    device->host = string("CoreMIDI"); // TODO cache
    return device;
}

inline static void delete_device(device_t device)
{
    fa_destroy(device->name);
    fa_destroy(device->host);
    fa_delete(device);
}

inline static stream_t new_stream(device_t device)
{        
    assert(false);
    stream_t stream         = fa_new(midi_stream);

    stream->impl            = &midi_stream_impl;
    stream->device          = device;

    // stream->native_input    = NULL;
    // stream->native_output   = NULL;

    // stream->message_callback_count = 0;
    
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
    gMidiMutex        = fa_thread_create_mutex();
    gMidiActive       = false;
    gMidiCurrentSession = NULL;
}

void fa_midi_terminate()
{
    fa_thread_destroy_mutex(gMidiMutex);
}

// --------------------------------------------------------------------------------

void status_listener(const MIDINotification *message, ptr_t x)
{                                                            
    session_t session = x;
    MIDINotificationMessageID id = message->messageID;

    printf("Called status_listener (you should NOT be seing this, please report it as a bug)\n");
    
    if (id == kMIDIMsgSetupChanged) {
        int n = session->callbacks.count;
        for (int i = 0; i < n; ++i) {
            nullary_t f = session->callbacks.elements[i].function;
            ptr_t     x = session->callbacks.elements[i].data;
            f(x);
        }
    }

    fa_with_lock(gMidiMutex)
    {         
        // Do things with mutex
    }
}

ptr_t session_thread(ptr_t x) {
    native_error_t result;
    session_t session = x;
    fa_print_ln(fa_string_show(fa_thread_current()));
                                                  
    // Save the loop so we can stop it from outside
    session->thread_abort = CFRunLoopGetCurrent();
    
    {
        // TODO cache name
        CFStringRef name = fa_string_to_native(string("faudio"));
        result = MIDIClientCreate(name, status_listener, session, &session->native);
        if (result) {
            warn(fa_string_format_integral("%d", result));
            // FIXME handle error
        }

        session_init_devices(session);

        CFRunLoopRun(); // TODO find a way to stop runLoop

        result = MIDIClientDispose(session->native);
        if (result) {
            warn(fa_string_format_integral("%d", result));
            // FIXME handle error
        } else {
            inform(string("(disposed client)"));
        }
        fa_thread_sleep(3000);
    }
    return NULL;
}

session_t fa_midi_begin_session()
{
    if (!gMidiMutex) {
        assert(false && "Module not initalized");
    }

    inform(string("Initializing real-time midi session"));

    midi_lock(gMidiMutex);
    {
        /* Assure no overlaps.
         */
        if (gMidiActive) {
            midi_unlock(gMidiMutex);
            return (session_t) midi_device_error(string("Overlapping real-time midi sessions"));
        } else {                                                 
            session_t session = new_session();
            /* Still need to init native, devices and threads (see above).
             */

            fa_print_ln(fa_string_show(fa_thread_current()));
            session->thread = fa_thread_create(session_thread, session);
            // TODO get error from thread
            
            // FIXME assure init_devices done
            fa_thread_sleep(500);

            gMidiActive         = true;
            gMidiCurrentSession = session;
            
            midi_unlock(gMidiMutex);
            return session;
        }
    }
}

void fa_midi_end_session(session_t session)
{
    if (!gMidiMutex) {
        assert(false && "Not initalized");
    }

    inform(string("Terminating real-time midi session"));

    midi_lock(gMidiMutex);
    {
        if (gMidiActive) {
            inform(string("(actually terminating)"));

            CFRunLoopStop(session->thread_abort);
            fa_thread_join(session->thread);
            // TODO get error from thread

            // FIXME this does *NOT* work, CoreMIDI will always use *one* thread
            // (the first registered) for all further notifications.
            
            gMidiActive = false;
            gMidiCurrentSession = NULL;
        }
    }
    midi_unlock(gMidiMutex);
    delete_session(session);

    inform(string("(finished terminating)"));
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

fa_list_t fa_midi_current_session()
{
    if (!gMidiCurrentSession) {
        return list();
    } else {
        return list(gMidiCurrentSession);
    }
}

fa_ptr_t fa_midi_end_all_sessions()
{
    fa_dfor_each(x, fa_midi_current_session()) {
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
        return (type) midi_device_error(string("No input device available")); \
    }
#define fail_if_no_output(type) \
    if (!session->def_output) { \
        return (type) midi_device_error(string("No output device available")); \
    }

fa_pair_t fa_midi_default(session_t session)
{
    fail_if_no_input(fa_pair_t);
    fail_if_no_output(fa_pair_t);
    return pair(session->def_input, session->def_output);
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

void fa_midi_add_status_callback(status_callback_t function,
                                 ptr_t             data,
                                 session_t         session)
{   
    int n = session->callbacks.count++;
    session->callbacks.elements[n].function = function;
    session->callbacks.elements[n].data     = data;
}

void fa_midi_add_message_callback(message_callback_t function,
                                  fa_ptr_t           data,
                                  fa_midi_stream_t   stream)
{
    int n = stream->callbacks.count++;
    stream->callbacks.elements[n].function = function;
    stream->callbacks.elements[n].data     = data;
}

fa_string_t fa_midi_name(device_t device)
{
    return fa_copy(device->name);
}

fa_string_t fa_midi_host_name(device_t device)
{
    return fa_copy(device->host);
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

// PmTimestamp midi_time_callback(void *data)
// {
//     stream_t stream = data;
//     return fa_clock_milliseconds(stream->clock);
// }

ptr_t stream_thread_callback(ptr_t x);

fa_midi_stream_t fa_midi_open_stream(device_t device)
{
    // allocate
    // if input
        // create input port that writes to all callbacks in this stream
        // connect native source
    // if output
        // create output port
        // make main thread propagate writes to port





    // if (!device) {
    //     return (stream_t) midi_device_error_with(
    //         string("Can not open a stream with no devices"), 0);
    // }

    midi_inform_opening(device);
    assert(false);
    // 
    // native_error_t result;
    // stream_t stream = new_stream(device);
    // 
    // if (device->input) {
    //     inform(string("Opening input\n"));
    //     // result = Pm_OpenInput(&stream->native_input, device->index, NULL, 0,
    //     //                       midi_time_callback, stream);
    //     result = 0; // FIXME
    // 
    //     if (result < 0) {
    //         midi_error(string("Could not open midi input"), result);
    //     }
    // }
    // 
    // if (device->output) {
    //     inform(string("Opening output\n"));
    //     // result = Pm_OpenOutput(&stream->native_output, device->index, NULL, 0,
    //     //                        midi_time_callback, stream, -1);
    //     result = 0; // FIXME
    // 
    //     if (result < 0) {
    //         midi_error(string("Could not open midi output"), result);
    //     }
    // }
    // 
    // // FIXME
    // // stream->thread = fa_thread_create(stream_thread_callback, stream);
    // return stream;  
}


void fa_midi_close_stream(stream_t stream)
{
    inform(string("Closing real-time midi stream"));

    // TODO instruct to stop
    // stream->thread_abort = true;
    // fa_thread_join(stream->thread);

    // if (stream->native_input) {
        // Pm_Close(stream->native_input);
        // FIXME
    // }

    // if (stream->native_output) {
        // Pm_Close(stream->native_output);
        // FIXME
    // }
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




// void read_in(PmEvent* dest, stream_t stream);

ptr_t stream_thread_callback(ptr_t x)
{                 
    stream_t stream = x;
    inform(string("  Midi service thread active"));

    while(1) {              
        // if (stream->thread_abort) {
            // inform(string("  Midi service thread finished"));
            // return 0;
        // }
        // inform(string("  Midi service thread!"));

        // Inputs
        // if (stream->native_input) {
            // while (Pm_Poll(stream->native_input)) {
            // 
            //     PmEvent events[1];
            //     read_in(events, stream);
            //     // Pm_Read(stream->native_input, events, 1); // TODO error
            //     
            //     for (int i = 0; i < stream->message_callback_count; ++i) {
            //         unary_t f = stream->message_callbacks[i];
            //         ptr_t   x = stream->message_callback_ptrs[i];
            //         
            //         time_t time = fa_milliseconds(events[0].timestamp);
            //         midi_message_t msg = midi_message(
            //             Pm_MessageStatus(events[0].message), 
            //             Pm_MessageData1(events[0].message), 
            //             Pm_MessageData2(events[0].message));
            // 
            //         f(x, pair(time, msg));
            //     }
            // }
            // FIXME
        // }

        // Outputs
        if (/*stream->native_output*/false) { // FIXME
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
        // fa_thread_sleep(kMidiServiceThreadInterval);
    }
    assert(false && "Unreachable");
}



void fa_midi_schedule(fa_time_t        time,
                      fa_action_t      action,
                      fa_midi_stream_t stream)
{
    pair_left_t pair = pair_left(time, action);
    fa_atomic_queue_write(stream->in_controls, pair);

    if (fa_action_is_send(action)) {
        string_t name = fa_action_send_name(action);
        ptr_t    value = fa_action_send_value(action);
        send_out(value, stream); // TODO laterz
        mark_used(name);
    }
}

            
// void read_in(PmEvent* dest, stream_t stream)
// {   
//     native_error_t result;
//     // result = Pm_Read(stream->native_input, dest, 1);
//     result = 0; // FIXME
// 
//     if (result < 0) {
//         midi_error(string("Could not fetch midi"), result);
//     }
// }

void send_out(midi_message_t midi, stream_t stream)
{
    native_error_t result;

    if (fa_midi_message_is_simple(midi)) {
        // timestamp ignored
        long midi_message = fa_midi_message_simple_to_long(midi);

        printf("Sending: %s %08x\n", unstring(fa_string_show(midi)), (int) midi_message);

        // result = Pm_WriteShort(stream->native_output, 0, midi_message);
        result = 0; // FIXME

        if (result < 0) {
            midi_error(string("Could not send midi"), result);
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
    return fa_equal(device1->name, device2->name);
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
error_t midi_error(string_t msg, int code)
{
    // string_t msg2 = string((char *) Pm_GetErrorText(code));
    string_t msg2 = string(""); // FIXME
    return fa_error_create_simple(error,
                                  string_dappend(msg, msg2),
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






