
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
#include <fa/util/macros.h>
#include <fa/dynamic.h>

#define NO_THREAD_T
#include <fa/util.h>
#undef NO_THREAD_T

#include <CoreMIDI/CoreMIDI.h>
#include <CoreServices/CoreServices.h>

#include "../shared/action_internal.h"


/*
    ## Notes

    * Implementation in terms of CoreMIDI

      CoreMIDI requires a dedicated thread+CFRunLoop to work properly, usually the main
      thread is used. This is not an option here, so instead we launch a *global* MIDI
      thread at initialization time. All CoreMIDI interaction must happen on this thread,
      and both status and MIDI callbacks are executed here.

      The begin/end session functions defer to the MIDI thread to start or stop a new
      session. All session data/devices etc. are copied into the session and read from
      other threads. The only mutable values in sessions are the callbacks.

    * Mapping:
        - Sessions <-> MIDIClients
            - Still requiring uniqueness
        - Devices <-> MIDIEndpoints
            - We do not provide any direct access to MIDIDevices/MIDIEnities
            - Each endpoint shows up as a *separate* device
        - Streams <-> MIDIPorts

     * Each MIDI stream has an internal clock which currently defaults to the
     fa_clock_standard(). We might add other options later, such as using the CoreMIDI time stamp.

 */

typedef fa_midi_session_t           session_t;
typedef fa_midi_device_t            device_t;
typedef fa_midi_stream_t            stream_t;
typedef fa_midi_session_callback_t  session_callback_t;
typedef fa_midi_stream_callback_t   stream_callback_t;
typedef fa_midi_status_callback_t   status_callback_t;
typedef fa_midi_message_callback_t  message_callback_t;
typedef fa_nullary_t                timer_callback_t;
typedef fa_action_t                 action_t;

typedef MIDIClientRef               native_session_t;
typedef MIDIEndpointRef             native_device_t;
typedef MIDIPortRef                 native_stream_t;
typedef OSStatus                    native_fa_error_t;

#define kMaxMessageCallbacks        64
#define kMaxStatusCallbacks         64
#define kMaxTimerCallbacks          512     // needs to be one per stream
#define kMidiOutIntervalSec         0.001
#define kSysexMaxSize               1024

struct _fa_midi_session_t {
    fa_impl_t                       impl;               // Dispatcher
    native_session_t                native;

    fa_list_t                       devices;            // Cached device list
    device_t                        def_input;          // Default devices, both possibly null
    device_t                        def_output;         // If present, these are also in the above list

    fa_list_t                       streams;            // All streams started on this sessiuon (list of stream_t)

    struct {
        int                         count;
        struct {
            fa_nullary_t            function;
            fa_ptr_t                data;
        }                           elements[kMaxTimerCallbacks];
    }                               timer_callbacks;    // Active streams

    struct {
        int                         count;
        struct {
            fa_nullary_t            function;
            fa_ptr_t                data;
        }                           elements[kMaxStatusCallbacks];
    }                               callbacks;          // Status callbacks
};

struct _fa_midi_device_t {
    fa_impl_t                       impl;               // Dispatcher
    native_device_t                 native;             // Native device
    session_t                       session;            // Enclosing session

    bool                            input, output;      // Cached capabilities
    fa_string_t                        name, host;         // Cached names
};

struct _fa_midi_stream_t {

    fa_impl_t                       impl;               // Dispatcher
    native_stream_t                 native;             // Native stream
    device_t                        device;             // Enclosing session

    fa_clock_t                      clock;              // Clock used for scheduler and incoming events
    fa_atomic_queue_t               in_controls;        // Controls for scheduling, (AtomicQueue (Time, (Channel, Ptr)))
    fa_atomic_queue_t               short_controls;     // Controls to run directly (optimization)
    fa_priority_queue_t             controls;           // Scheduled controls (Time, (Channel, Ptr))
    int                             timer_id;           // Used to unregister timer callbacks

    bool                            in_sysex;           // Currently receiving a SysEx message
    struct {
        uint8_t                     data[kSysexMaxSize];
        size_t                      count;
    }                               sysex_in_buffer;

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
static fa_thread_mutex_t            gMidiMutex;

static bool                         gMidiActive;
static bool                         gMidiTerminating;

static session_t                    gPendingSession;
static session_t                    gMidiCurrentSession;
static fa_thread_t                  gMidiThread;
static CFRunLoopRef                 gMidiThreadRunLoop;
static native_session_t             gMidiClient = 0;


fa_error_t midi_device_error(fa_string_t msg);
fa_error_t midi_device_error_with(fa_string_t msg, native_fa_error_t error);
fa_error_t midi_error(fa_string_t msg, native_fa_error_t code);
void midi_device_fatal(fa_string_t msg, native_fa_error_t code);
fa_ptr_t midi_session_impl(fa_id_t interface);
fa_ptr_t midi_device_impl(fa_id_t interface);
fa_ptr_t midi_stream_impl(fa_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(bool is_output, native_device_t native, session_t session);
inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t device);
inline static void delete_stream(stream_t stream);
long fa_midi_message_simple_to_long(fa_midi_message_t midi);


// --------------------------------------------------------------------------------

/** Intializes everything except native and devices.
 */
inline static session_t new_session()
{
    session_t session   = fa_new(midi_session);
    session->impl       = &midi_session_impl;
    session->native     = 0;

    session->callbacks.count = 0;
    session->timer_callbacks.count = 0;
    session->streams = fa_empty();

    return session;
}

/** Intializes def_input, def_output and devices.
 */
inline static void session_init_devices(session_t session)
{
    fa_list_t         devices = fa_empty();
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

        // printf("OTHER: %d\n", kMIDIPropertyName);
        if (!kMIDIPropertyName) {
            printf("kMIDIPropertyName not set, aborted\n");
            exit(-1);
        }

        if (MIDIObjectGetStringProperty(native, kMIDIPropertyName, &name)) {
            assert(false && "Could not get name");
        }

        device->name = fa_string_from_native((void *) name);
    }
    device->host = fa_string("CoreMIDI");
    return device;
}

inline static void delete_device(device_t device)
{
    fa_destroy(device->name);
    fa_destroy(device->host);
    fa_delete(device);
}

// native
inline static stream_t new_stream(device_t device)
{
    stream_t stream         = fa_new(midi_stream);

    stream->impl            = &midi_stream_impl;
    stream->device          = device;

    stream->clock           = fa_clock_standard();
    stream->in_controls     = fa_atomic_queue();
    stream->short_controls  = fa_atomic_queue();
    stream->controls        = fa_priority_queue();

    stream->callbacks.count = 0;
    stream->in_sysex = false;
    stream->sysex_in_buffer.count = 0;

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


/*
    gMidiActive
        Set to false from start.
        Set to true by user thread after starting a new session.
        Set to false by MIDI thread to indicate session stopped.

    gPendingSession
        Set to kNoSession from start.
        Set to kRequestSession by user thread when a new session should be created
        Set to the session by the MIDI thread to indicate session started
*/


/*
    kRequestSession
        Synonym for null.
    kNoSession
        A non-null value which is not a valid pointer either
*/
#define kNoSession      ((session_t) 1)
#define kRequestSession ((session_t) 0)

void status_listener(const MIDINotification *message, fa_ptr_t data)
{
    session_t session = gMidiCurrentSession; //data;
    if (!session) return;
    MIDINotificationMessageID id = message->messageID;

    // printf("Called status_listener (if you see this, please report it as a bug)\n");

    if (id == kMIDIMsgSetupChanged) {
        int n = session->callbacks.count;

        for (int i = 0; i < n; ++i) {
            fa_nullary_t f = session->callbacks.elements[i].function;
            fa_ptr_t     x = session->callbacks.elements[i].data;
            f(x);
        }
    }
}

static void midi_timer(CFRunLoopTimerRef timer, void *data)
{
    session_t session = data;

    // printf("Called midi_timer (if you see this, please report it as a bug)\n");

    int n = session->timer_callbacks.count;

    for (int i = 0; i < n; ++i) {
        fa_nullary_t f = session->timer_callbacks.elements[i].function;
        fa_ptr_t     x = session->timer_callbacks.elements[i].data;

        if (f && x) {
            f(x);
        }
    }


}

fa_ptr_t midi_thread(fa_ptr_t x)
{
    session_t session = NULL;

    // MIDIRestart(); // Commented out 2019-04-10

    // Save the loop so we can stop it from outside
    gMidiThreadRunLoop = CFRunLoopGetCurrent();

    fa_inform(fa_string("    CoreMIDI interaction thread active"));

    int result = MIDIClientCreate(fa_string_to_native(fa_string("faudio")), status_listener, NULL, &gMidiClient);
    if (result) {
        fa_fail(fa_string_format_integral("Could not start CoreMIDI", result));
        assert(false);
    }

    // Until faudio is terminated, this thread will repeatedly wait for instructions
    // to start a session, and act as its run loop.

    while (true) {
        fa_thread_sleep(100);

        // Only proceed when gPendingSession becomes zero
        if (gPendingSession != kRequestSession) {
            continue;
        }

        if (gMidiTerminating) {
            break;
        }

        {
            session = new_session(); // Still have to set native and devices
            session->native = gMidiClient;

            // This sets devices
            session_init_devices(session);
            gPendingSession = session;
        }

        {
            CFRunLoopTimerContext ctxt;
            ctxt.info = session;
            ctxt.version = 0;
            ctxt.retain = NULL;
            ctxt.release = NULL;
            ctxt.copyDescription = NULL;

            // printf("kCFAllocatorDefault is %p\n", kCFAllocatorDefault);

            CFRunLoopTimerRef timer = CFRunLoopTimerCreate(
                                          kCFAllocatorDefault,
                                          0,
                                          kMidiOutIntervalSec,
                                          0,
                                          0,
                                          midi_timer,
                                          &ctxt
                                      );
            CFRunLoopAddTimer(CFRunLoopGetCurrent(), timer, kCFRunLoopCommonModes);

            // We will be stuck here until the run loop is stopped
            // This only happen when a session is ended
            CFRunLoopRun();

            CFRunLoopRemoveTimer(CFRunLoopGetCurrent(), timer, kCFRunLoopCommonModes);
            CFRelease(timer);
        }

        gMidiActive = false;
    }

    result = MIDIClientDispose(gMidiClient);
    if (result) {
        fa_warn(fa_string_format_integral("Could not stop CoreMIDI, MIDIClientDispose failed with %d", result));
    } else {
        fa_inform(fa_string("(disposed client)"));
    }
    gMidiClient = 0;

    fa_inform(fa_string("Disposing CoreMIDI interaction thread"));

    // Successfully terminated
    return NULL;
}

void fa_midi_initialize()
{
    gMidiMutex          = fa_thread_create_mutex();

    gMidiActive         = false;
    gMidiTerminating    = false;

    gPendingSession     = kNoSession;
    gMidiCurrentSession = NULL;
    gMidiThreadRunLoop  = NULL;
    gMidiClient         = 0;

    gMidiThread         = fa_thread_create(midi_thread, NULL, fa_string("MIDI Controller Thread"));

    fa_inform(fa_string("    Using CoreMIDI as MIDI backend."));

    while (!gMidiThreadRunLoop) {
        fa_thread_sleep(10); // Wait for run loop to be initialized by thread
    }
}

void fa_midi_terminate()
{
    fa_midi_end_all_sessions();
    gMidiTerminating = true;
    gPendingSession  = kRequestSession; // Wake up
    fa_thread_join(gMidiThread);

    fa_thread_destroy_mutex(gMidiMutex);

}

#pragma mark ---------------------------------------------------------------------------
#pragma mark Session start and stop
#pragma mark ---------------------------------------------------------------------------

#define assert_module_initialized() \
    if (!gMidiMutex) { \
        assert(false && "Not initialized"); \
    }

session_t fa_midi_begin_session()
{
    assert_module_initialized();
    fa_inform(fa_string("Initializing real-time midi session"));

    session_t session;

    fa_with_lock(gMidiMutex)
    {
        // Assure no overlaps.
        if (gMidiActive) {
            session = (session_t) midi_device_error(fa_string("Overlapping real-time midi sessions"));
        } else {
            // Wake up MIDI thread
            gPendingSession = kRequestSession;

            // Wait for the MIDI thread to start the new session
            while (!gPendingSession) {
                fa_thread_sleep(10);
            }

            session             = gPendingSession;
            gMidiActive         = true;
            gMidiCurrentSession = session;
        }
    }
    return session;
}

void fa_midi_end_session(session_t session)
{
    assert_module_initialized();
    fa_inform(fa_string("Terminating real-time midi session"));

    fa_inform(fa_string("    Closing streams"));
    fa_for_each(stream, session->streams) {
        // It is OK if the stream is already closed
        fa_midi_close_stream(stream);
        delete_stream(stream);
    }
    fa_inform(fa_string("    Stopping driver"));

    fa_with_lock(gMidiMutex)
    {
        if (gMidiActive) {
            fa_inform(fa_string("    Actually terminating"));
            CFRunLoopStop(gMidiThreadRunLoop);

            // Wait for the session to end
            while (gMidiActive) {
                fa_thread_sleep(10);
            }

            gMidiCurrentSession = NULL;
        }
    }
    fa_inform(fa_string("    Finished terminating midi session"));
}

void fa_midi_with_session(session_callback_t    session_callback,
                          fa_ptr_t              session_data,
                          fa_error_callback_t      error_callback,
                          fa_ptr_t              error_data)
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
    if (!gMidiCurrentSession) {
        return list();
    } else {
        return list(gMidiCurrentSession);
    }
}

fa_ptr_t fa_midi_end_all_sessions()
{
    fa_dfor_each(x, fa_midi_current_sessions()) {
        fa_midi_end_session(x);
    }
    return NULL;
}


#pragma mark ---------------------------------------------------------------------------
#pragma mark Introspection
#pragma mark ---------------------------------------------------------------------------

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


int  fa_midi_add_timer_callback(timer_callback_t function,
                                fa_ptr_t            data,
                                session_t        session)
{
    int n = session->timer_callbacks.count++;
    session->timer_callbacks.elements[n].function = function;
    session->timer_callbacks.elements[n].data     = data;
    return n;
}

void fa_midi_remove_timer_callback(int              index,
                                   session_t        session)
{
    session->timer_callbacks.elements[index].function = NULL;
    session->timer_callbacks.elements[index].data     = NULL;
}


void fa_midi_add_status_callback(status_callback_t function,
                                 fa_ptr_t             data,
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


#pragma mark ---------------------------------------------------------------------------
#pragma mark Stream start and stop
#pragma mark ---------------------------------------------------------------------------

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


void forward_fa_message_to_callbacks(fa_midi_stream_t stream, fa_time_t time, fa_ptr_t msg)
{
    int n = stream->callbacks.count;

    for (int j = 0; j < n; ++j) {
        fa_unary_t f = stream->callbacks.elements[j].function;
        fa_ptr_t   x = stream->callbacks.elements[j].data;

        f(x, fa_pair_create(time, msg));
    }
}

void reset_sysex_buffer(stream_t stream)
{
    stream->sysex_in_buffer.count = 0;
}

void push_sysex_byte(stream_t stream, uint8_t x)
{
    if (stream->sysex_in_buffer.count >= kSysexMaxSize) {
        fa_warn(fa_string("Sysex overflow!"));
    }

    stream->sysex_in_buffer.data[stream->sysex_in_buffer.count++] = x;
}

fa_buffer_t copy_sysex_to_new_buffer(stream_t stream)
{
    size_t size = stream->sysex_in_buffer.count;
    void *data = fa_malloc(size);
    memcpy(data, (void *) stream->sysex_in_buffer.data, size);
    return fa_buffer_dwrap(data, size);
}

void message_listener(const MIDIPacketList *packetList, fa_ptr_t x, fa_ptr_t _)
{
    stream_t stream = x;
    // printf("Called status_listener (if you see this, please report it as a bug)\n");

    // We currently do not tolerate SysEx distributed over many packets
    // Do an extra reset here just in case
    reset_sysex_buffer(stream);

    // Use stream clock for time stamp
    // We could get time from time stamp for more relative precision, but that would
    // require an extra step to with the stream clock.
    fa_time_t time = fa_clock_time(stream->clock);


    // From the CoreMIDI documentation:
    // Note that the packets in the list, while defined as an array,
    // may not be accessed as an array, since they are variable-length.
    // To iterate through the packets in a packet list, use [...] MIDIPacketNext
    const MIDIPacket *packet = &packetList->packet[0];
    for (int i = 0; i < packetList->numPackets; ++i) {
        char status = 0, data1 = 0, data2 = 0;
        enum {
            init,
            await_data1,
            await_data2,
            in_sysex
        } state = init;

        // Iterate over every byte in the packet
        for (int j = 0; j < packet->length; ++j) {
            bool open_sysex  = packet->data[j] == 0xf0;
            bool close_sysex = packet->data[j] == 0xf7;

            if (open_sysex) {
                if (state != init) {
                    fa_warn(fa_string("Bad MIDI syntax"));
                }

                push_sysex_byte(stream, packet->data[j]);
                state = in_sysex;
            } else if (close_sysex) {
                if (state != in_sysex) {
                    fa_warn(fa_string("Bad MIDI syntax"));
                }

                push_sysex_byte(stream, packet->data[j]);
                {
                    fa_midi_message_t msg = fa_midi_message_create_sysex(copy_sysex_to_new_buffer(stream));
                    forward_fa_message_to_callbacks(stream, fa_copy(time), msg);
                    reset_sysex_buffer(stream);
                }
                state = init;
            } else if (state == in_sysex) {
                push_sysex_byte(stream, packet->data[j]);
                state = in_sysex; // no change
            } else {
                // printf("state=%d, data=%d\n", state, packet->data[j]);
                switch (state) {
                case init:
                    status = packet->data[j];
                    state = await_data1;
                    continue;

                case await_data1:
                    data1 = packet->data[j];
                    state = await_data2;
                    continue;

                case await_data2:
                    data2 = packet->data[j];
                    forward_fa_message_to_callbacks(stream, fa_copy(time), fa_midi_message(status, data1, data2));
                    state = init;
                    continue;

                default:
                    fa_warn(fa_string("Bad MIDI syntax"));
                    continue;
                }

            }

            packet = MIDIPacketNext(packet);
        }
    }

    fa_destroy(time);
}

static inline bool is_two_byte_message(uint8_t status)
{
    return ((status & 0xF0) == 0xC0) || ((status & 0xF0) == 0xD0);
}

fa_ptr_t forward_action_to_midi(fa_ptr_t x, fa_ptr_t action, fa_ptr_t time)
{
    stream_t stream = x;

    if (fa_action_is_compound(action)) {
        fa_warn(fa_string_dappend(fa_string("Compound action passed to Midi.forwardActionToMidi: "), fa_string_show(action)));
        return NULL;
    }

    if (fa_action_is_send(action)) {
        // fa_string_t name   = fa_action_send_name(action);
        fa_ptr_t message     = fa_action_send_value(action);

        if (fa_midi_message_is_simple(message)) {
            uint8_t sc, d1, d2;
            fa_midi_message_decons(message, &sc, &d1, &d2);

            {
                // printf("%d %d %d\n", sc, d1, d2);

                struct MIDIPacketList packetList; // Max one packet
                packetList.numPackets = 1;

                packetList.packet[0].timeStamp = 0;

                if (is_two_byte_message(sc)) {
                    packetList.packet[0].length = 2;
                } else {
                    packetList.packet[0].length = 3;
                }

                packetList.packet[0].data[0] = sc;
                packetList.packet[0].data[1] = d1;
                packetList.packet[0].data[2] = d2;

                native_fa_error_t result = MIDISend(
                                               stream->native,
                                               stream->device->native,
                                               &packetList
                                           );

                if (result < 0) {
                    fa_warn(fa_string("Could not send MIDI"));
                    fa_action_destroy(action);
                    return 0;
                }
            }
        } else {
            // fa_warn(fa_string("Could not send SysEx"));
            fa_buffer_t data = fa_midi_message_sysex_data(message);

            if (fa_buffer_size(data) > 256) {
                fa_warn(fa_string("Too large SysEx to send, ignoring!"));
                fa_action_destroy(action);
                return 0;
            }

            struct MIDIPacketList packetList; // Max one packet

            packetList.numPackets = 1;

            packetList.packet[0].timeStamp = 0;

            packetList.packet[0].length = fa_buffer_size(data); // Max 256

            for (int i = 0; i < fa_buffer_size(data); ++i) {
                uint8_t b = fa_buffer_get(data, i);
                packetList.packet[0].data[i] = b;
            }

            native_fa_error_t result = MIDISend(
                                           stream->native,
                                           stream->device->native,
                                           &packetList
                                       );

            if (result < 0) {
                fa_warn(fa_string("Could not send MIDI"));
                fa_action_destroy(action);
                return 0;
            }
        }

        fa_action_destroy(action);
        return NULL;
    }

    fa_warn(fa_string_dappend(fa_string("Unknown simple action passed to Midi.forwardActionToMidi: "), fa_string_show(action)));
    fa_action_destroy(action);
    return NULL;
}

fa_ptr_t midi_stream_callback(fa_ptr_t x)
{
    stream_t stream = x;

    fa_ptr_t val;

    while ((val = fa_atomic_queue_read(stream->short_controls))) {
        forward_action_to_midi(stream, val, NULL);
    }

    while ((val = fa_atomic_queue_read(stream->in_controls))) {
        fa_priority_queue_insert(fa_pair_left_from_pair(val), stream->controls);
    }

    fa_time_t   now    = fa_clock_time(stream->clock);
    run_actions(stream->controls,
                now,
                forward_action_to_midi,
                stream,
                kScheduleLookahead
               );
    fa_destroy(now);

    return NULL;
}



fa_midi_stream_t fa_midi_open_stream(device_t device)
{
    midi_fa_inform_opening(device);

    stream_t stream = new_stream(device);

    /* Note: we assume that either device->input or device->output is set but not both.
     */
    if (device->input) {
        MIDIInputPortCreate(
            device->session->native,
            fa_string_to_native(fa_string("faudio")),
            message_listener,
            stream,
            &stream->native
        );
        MIDIPortConnectSource(stream->native, device->native, NULL);
    }

    if (device->output) {
        MIDIOutputPortCreate(
            device->session->native,
            fa_string_to_native(fa_string("faudio")),
            &stream->native
        );
        stream->timer_id = fa_midi_add_timer_callback(midi_stream_callback, stream, stream->device->session);
    }

    fa_push_list(stream, device->session->streams);
    return stream;
}


void fa_midi_close_stream(stream_t stream)
{
    fa_inform(fa_string("Closing real-time midi stream"));

    if (stream->device->input) {
        MIDIPortDisconnectSource(stream->native, stream->device->native);
        MIDIPortDispose(stream->native);
    }

    if (stream->device->output) {
        MIDIPortDispose(stream->native);
        // TODO verify that this is atomic
        fa_midi_remove_timer_callback(stream->timer_id, stream->device->session);
    }
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

void fa_midi_schedule(fa_time_t        time,
                      fa_action_t      action,
                      fa_midi_stream_t stream)
{
    fa_pair_left_t pair = fa_pair_left_create(time, action);
    // Pass to scheduler
    fa_atomic_queue_write(stream->in_controls, pair);
}

void fa_midi_schedule_relative(fa_time_t        time,
                               fa_action_t       action,
                               fa_midi_stream_t  stream)
{
    fa_time_t now = fa_clock_time(stream->clock);
    fa_midi_schedule(fa_dadd(now, time), action, stream);
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

fa_clock_t fa_midi_get_clock(fa_midi_stream_t stream)
{
    return stream->clock;
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
    // TODO check that sessions are valid
    return fa_equal(device1->name, device2->name);
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

fa_error_t midi_device_error_with(fa_string_t msg, native_fa_error_t code)
{
    return fa_error_create_simple(error,
                                  fa_string_dappend(msg, fa_string_format_integral(" (error code %d)", code)),
                                  fa_string("Doremir.Device.Midi"));
}

// TODO consolidate
fa_string_t from_os_status2(OSStatus err)
{
    return fa_string_from_utf8((char *) GetMacOSStatusErrorString(err));
}

fa_error_t midi_error(fa_string_t msg, native_fa_error_t code)
{
    fa_string_t msg2 = from_os_status2(code);
    return fa_error_create_simple(error,
                                  fa_string_dappend(msg, msg2),
                                  fa_string("Doremir.Device.Midi"));
}


void midi_device_fatal(fa_string_t msg, native_fa_error_t code)
{
    fa_log_error_from(
        fa_string_dappend(msg, fa_string_format_integral(" (error code %d)", code)),
        fa_string("Doremir.Device.Midi"));

    fa_log_error(fa_string("Terminating Fa"));
    exit(error);
}






