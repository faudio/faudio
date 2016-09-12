
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/audio.h>

#include <fa/atomic.h>
#include <fa/dynamic.h>
#include <fa/atomic/queue.h>
#include <fa/pair/left.h>
#include <fa/priority_queue.h>
#include <fa/signal.h>
#include <fa/thread.h>
#include <fa/util.h>
#include <fa/time.h>
#include <pthread.h>
#include <portaudio.h>
#if _WIN32
#include <pa_win_wasapi.h>
#endif
#include "signal.h"
#include "signal_internal.h"
#include "action_internal.h"

/*
    Notes:
        * Name mixup. What to call incoming/outcoming to make it unambigous.
            * to_audio, from_audio or something?
        * FIXME endSession must check for running streams, or we get consistency errors
            * Either abort streams or refuse to close session
 */

typedef fa_audio_proc_t             proc_t;
typedef fa_audio_device_t           device_t;
typedef fa_audio_stream_t           stream_t;
typedef fa_audio_session_t          session_t;
typedef fa_audio_stream_callback_t  stream_callback_t;
typedef fa_audio_session_callback_t session_callback_t;
typedef fa_audio_status_callback_t  status_callback_t;
typedef fa_signal_custom_processor_t   *custom_proc_t;

typedef PaDeviceIndex native_index_t;
typedef PaHostApiIndex native_host_t;
typedef PaStream     *native_stream_t;

#define kMaxSignals             32
#define kMaxMessageCallbacks    64
#define kMaxStatusCallbacks     64

struct _fa_audio_session_t {

    fa_impl_t           impl;               // Dispatcher
    system_time_t       acquired;           // Time of acquisition (not used at the moment)

    fa_list_t           devices;            // Cached device list

    device_t            def_input;          // Default devices, both possibly null
    device_t            def_output;         // If present, these are also in the above list

    fa_list_t           streams;            // All streams started on this sessiuon (list of stream_t)

    fa_list_t           host_names;         // Cached host list
    fa_string_t         default_host_name;  // Name of preferred audio host

    struct {
        double               sample_rate;
        double               latency_ex[2];       // Suggested latency for exclusive mode (only used with WASAPI)
        double               latency_sh[2];       // Suggested latency for shared mode
        int                  vector_size_ex;      // Vector size for exclusive mode (only used with WASAPI)
        int                  vector_size_sh;      // Vector size for shared mode
        int                  scheduler_interval;  // Scheduling interval in milliseconds
        fa_exclusive_mode_t  exclusive;           // Use exclusive mode (if available)
    }                        parameters;          // Parameters, which may be updated by set_parameter

    struct {
        int                         count;
        struct {
            fa_nullary_t            function;
            fa_ptr_t                data;
        }                           elements[kMaxStatusCallbacks];
    }                               callbacks;          // Status callbacks

    fa_pair_t                       status_closure;
};

struct _fa_audio_device_t {

    fa_impl_t           impl;               // Dispatcher
    native_index_t      index;              // Native device index
    native_host_t       host;
    session_t           session;            // Underlying session

    fa_string_t         name;               // Cached names
    fa_string_t         host_name;

    // bool                muted;           // Not used at the moment
    // double              volume;
};

struct _fa_audio_stream_t {

    fa_impl_t           impl;               // Dispatcher
    native_stream_t     native;             // Native stream, or NULL if closed
    device_t            input, output;

    unsigned            signal_count;       // Number of signals (same as number of outputs)
    fa_signal_t         signals[kMaxSignals];
    state_t             state;              // DSP state
    int64_t             last_time;          // Cached time in milliseconds
    double              roundtrip_latency;  // "Actual" latency calculated from time values reported by PA
    bool                exclusive_mode;     // Is exclusive mode actually used
    
    //int64_t             start_time;

    unsigned            input_channels, output_channels;
    double              sample_rate;
    long                max_buffer_size;
    PaStreamCallbackFlags pa_flags;         // Potential error messages from PortAudio

    struct {
        fa_thread_t     thread;
        // fa_thread_mutex_t         mutex;
        bool            stop;
    }                   controller;         // Controller thread (where scheduling runs)

    fa_atomic_queue_t   before_controls;    // Non-scheduled controls

    fa_atomic_queue_t   in_controls;        // From scheduler to audio (AtomicQueue SomeAction)
    fa_atomic_queue_t   short_controls;     // Directly to audio (AtomicQueue SomeAction)
    fa_atomic_queue_t   out_controls;       // Audio to scheduler (AtomicQueue SomeAction)

    fa_priority_queue_t controls;           // Scheduling queue (PriorityQueue (Time, Action))

    struct {
        int             count;
        struct {
            fa_binary_t function;
            fa_ptr_t    data;
        }               elements[kMaxMessageCallbacks];
    }                   callbacks;          // Message callbacks
    
    fa_pair_t           closed_callback;
};

static fa_thread_mutex_t   pa_mutex;
static bool      pa_status;
static session_t current_session;

fa_error_t audio_device_error(fa_string_t msg);
fa_error_t audio_device_error_with(fa_string_t msg, int error);
fa_ptr_t audio_session_impl(fa_id_t interface);
fa_ptr_t audio_device_impl(fa_id_t interface);
fa_ptr_t audio_stream_impl(fa_id_t interface);
inline static session_t new_session();
inline static void session_init_devices(session_t session);
inline static void delete_session(session_t session);
inline static device_t new_device(session_t session, native_index_t index);
//inline static void delete_device(device_t device);
inline static stream_t new_stream(device_t input, device_t output, double sample_rate);
inline static void delete_stream(stream_t stream);

void before_processing(stream_t stream);
void after_processing(stream_t stream);
void after_failed_processing(stream_t stream);
void during_processing(stream_t stream, unsigned count, double time, float **input, float **output);

static int native_audio_callback(const void *input_ptr,
                                 void *output_ptr,
                                 unsigned long frame_count,
                                 const PaStreamCallbackTimeInfo *time_info,
                                 PaStreamCallbackFlags flags,
                                 void *data);

static void native_finished_callback(void *data);

// --------------------------------------------------------------------------------

fa_ptr_t _status_callback(fa_ptr_t x)
{
    session_t session = (session_t) x;

    int n = session->callbacks.count;

    for (int i = 0; i < n; ++i) {
        fa_nullary_t f = session->callbacks.elements[i].function;
        fa_ptr_t     x = session->callbacks.elements[i].data;
        f(x);
    }

    return x;
}

inline static session_t new_session(fa_string_t default_host_name)
{
    session_t session = fa_new(audio_session);
    session->impl = &audio_session_impl;
    session->callbacks.count = 0;
    session->default_host_name = default_host_name;
    return session;
}

inline static void session_init_devices(session_t session)
{
    native_index_t count    = Pa_GetDeviceCount();
    fa_list_t      devices  = fa_empty();

    fa_slog_info("    Available Audio Devices:");
    for (size_t i = 0; i < count; ++i) {
        device_t device = new_device(session, i);

        if (device) {
            fa_slog_info("      ", device);
            devices = fa_list_dcons(device, devices);
        }
    }

    session->devices      = fa_list_dreverse(devices);
    session->streams      = fa_empty();

    // Default devices
    bool is_set = false;
    if (session->default_host_name) {
        
        fa_slog_info("    Preferred host:  ", session->default_host_name);
        
        PaHostApiIndex count = Pa_GetHostApiCount();
        bool found_host = false;
        for (PaHostApiIndex i = 0; i < count; i++) {
            const PaHostApiInfo *info = Pa_GetHostApiInfo(i);
            if (fa_dequal(fa_string(info->name), fa_copy(session->default_host_name))) {
                session->def_input = new_device(session, info->defaultInputDevice);
                session->def_output = new_device(session, info->defaultOutputDevice);
                is_set = true;
                found_host = true;
                break;
            }
        }
        if (found_host) {
            if (!session->def_input && !session->def_output) {
                fa_warn(fa_dappend(
                    fa_copy(session->default_host_name),
                    fa_string(" has no default input or output, falling back to default host")));
                    is_set = false;
            } else if (!session->def_input) {
                fa_warn(fa_dappend(fa_copy(session->default_host_name), fa_string(" has no default input")));
            } else if (!session->def_output) {
                fa_warn(fa_dappend(fa_copy(session->default_host_name), fa_string(" has no default output")));
            }
        } else {
            fa_slog_error("      Preferred host not found: ", session->default_host_name);
        }
    } else {
        fa_slog_info("    Preferred host: (none)");
    }
    
    if (!is_set) {
        session->def_input   = new_device(session, Pa_GetDefaultInputDevice());
        session->def_output  = new_device(session, Pa_GetDefaultOutputDevice());
    }

    fa_slog_info("    Default input:   ", session->def_input);
    fa_slog_info("    Default output:  ", session->def_output);

    session->parameters.sample_rate         = kDefSampleRate;
    session->parameters.scheduler_interval  = kAudioSchedulerIntervalMillis;
    session->parameters.vector_size_ex      = kDefVectorSize;
    session->parameters.vector_size_sh      = kDefVectorSize;
    session->parameters.latency_ex[0]       = kDefLatency;
    session->parameters.latency_ex[1]       = kDefLatency;
    session->parameters.latency_sh[0]       = kDefLatency;
    session->parameters.latency_sh[1]       = kDefLatency;
    session->parameters.exclusive           = em_never;
}

inline static void delete_session(session_t session)
{
    // TODO
    // Cannot destoy the device references, since they may be referenced
    // from the outside world. The solution is probably to always give away
    // copies of the devices.
    
    //if (session->def_input)         fa_destroy(session->def_input);
    //if (session->def_output)        fa_destroy(session->def_output);
    //if (session->devices)           fa_deep_destroy_always(session->devices);
    if (session->devices)           fa_destroy(session->devices); // only the list
    if (session->default_host_name) fa_destroy(session->default_host_name);
    fa_delete(session);
}

inline static device_t new_device(session_t session, native_index_t index)
{
    if (index == paNoDevice) {
        return NULL;
    }

    device_t device = fa_new(audio_device);
    device->impl    = &audio_device_impl;

    const PaDeviceInfo  *info      = Pa_GetDeviceInfo(index);
    const PaHostApiInfo *host_info = Pa_GetHostApiInfo(info->hostApi);

    device->index       = index;
    device->host        = info->hostApi;
    device->session     = session;

    /*
        PortAudio aspire to return UTF-8 on all platforms but this is not always the case
        Assume native encodings for now.

        See also #96
     */
#ifdef _WIN32
    device->name        = fa_string_from_cp1252((char *) info->name);       // const cast
    device->host_name   = fa_string_from_cp1252((char *) host_info->name);
#else
    device->name        = fa_string_from_utf8((char *) info->name);         // const cast
    device->host_name   = fa_string_from_utf8((char *) host_info->name);
#endif

    return device;
}

// inline static void delete_device(device_t device)
// {
//     fa_destroy(device->name);
//     fa_destroy(device->host_name);
//     fa_delete(device);
// }

inline static stream_t new_stream(device_t input, device_t output, double sample_rate)
{
    stream_t stream           = fa_new(audio_stream);

    stream->impl              = &audio_stream_impl;

    stream->input             = input;
    stream->output            = output;
    stream->input_channels    = (!input) ? 0 : fa_audio_input_channels(input);
    stream->output_channels   = (!output) ? 0 : fa_audio_output_channels(output);

    stream->sample_rate       = sample_rate;
    stream->max_buffer_size   = 0; // set later
    stream->state             = NULL;
    stream->last_time         = 0;
    //stream->start_time      = 0;
    stream->roundtrip_latency = 0;
    stream->exclusive_mode    = false;

    stream->signal_count      = 0;
    stream->pa_flags          = 0;

    stream->before_controls   = fa_atomic_queue();
    stream->in_controls       = fa_atomic_queue();
    stream->short_controls    = fa_atomic_queue();
    stream->controls          = fa_priority_queue();

    stream->out_controls      = fa_atomic_queue();

    stream->callbacks.count   = 0;

    return stream;
}

inline static void delete_stream(stream_t stream)
{
    fa_destroy(stream->before_controls);
    fa_destroy(stream->in_controls);
    fa_destroy(stream->out_controls);
    fa_destroy(stream->short_controls);
    fa_destroy(stream->controls);
    fa_delete(stream);
}


// --------------------------------------------------------------------------------

void fa_audio_initialize()
{
    pa_mutex        = fa_thread_create_mutex();
    pa_status       = false;
    current_session = NULL;

    if (kVectorMode) {
        fa_inform(fa_string("    Using vector processing"));
    } else {
        fa_inform(fa_string("    Using single-step processing"));
    }
}

void fa_audio_terminate()
{
    fa_thread_destroy_mutex(pa_mutex);
}

// --------------------------------------------------------------------------------

void add_audio_status_listener(fa_pair_t closure);
void remove_audio_status_listener(fa_pair_t closure);

session_t fa_audio_begin_session()
{
    return fa_audio_begin_session_with_preferred_host(NULL);
}

session_t fa_audio_begin_session_with_preferred_host(fa_string_t preferred_host_name)
{
    if (!pa_mutex) {
        assert(false && "Module not initalized");
    }

    fa_inform(fa_string("Initializing real-time audio session"));

    session_t session;
    fa_with_lock(pa_mutex) {
        if (pa_status) {
            session = (session_t) audio_device_error(fa_string("Overlapping real-time audio sessions"));
        } else {
            fa_slog_info("    Starting up PortAudio");
            Pa_Initialize();
            pa_status = true;
            fa_slog_info("    Done starting PortAudio");
            
            session = new_session(preferred_host_name);
            
            PaHostApiIndex count = Pa_GetHostApiCount();
            session->host_names = fa_empty();
            for (PaHostApiIndex i = 0; i < count; i++) {
                const PaHostApiInfo *info = Pa_GetHostApiInfo(i);
                fa_string_t host_name = fa_string(info->name);
                fa_push_list(host_name, session->host_names);
            }
            fa_slog_info("    Available hosts: ", session->host_names);
            
            session_init_devices(session);

            current_session = session;
        }
    }
    // FIXME cache pair
    session->status_closure = fa_pair_create(_status_callback, session);
    add_audio_status_listener(session->status_closure);

    fa_inform(fa_string("Done initializing session"));
    return session;
}

void fa_audio_end_session(session_t session)
{
    if (!pa_mutex) {
        assert(false && "Module not initalized");
    }

    fa_inform(fa_string("Terminating real-time audio session"));

    fa_inform(fa_string("   Unregistering hot-plug callbacks"));
    remove_audio_status_listener(session->status_closure);
    fa_inform(fa_string("   Finished unregistering hot-plug callbacks"));

    fa_with_lock(pa_mutex) {
        fa_for_each(stream, session->streams) {
            // It is OK if the stream is already closed
            fa_audio_close_stream(stream);
            delete_stream(stream);
        }

        if (pa_status) {
            Pa_Terminate();
            pa_status = false;
        }

        current_session = NULL;
    }

    fa_inform(fa_string("Finished terminating session"));
    fa_deep_destroy_always(session->host_names);
    delete_session(session);
}

void fa_audio_with_session(session_callback_t session_callback,
                           fa_ptr_t                           session_data,
                           fa_error_callback_t                error_callback,
                           fa_ptr_t                           error_data
                          )
{
    session_t session = fa_audio_begin_session();

    if (fa_check(session)) {
        error_callback(error_data, (fa_error_t) session);
    } else {
        session_callback(session_data, session);
    }

    fa_audio_end_session(session);
}

static inline void set_parameter(const char *name, fa_ptr_t value, session_t session)
{
    if (strcmp(name, "sample-rate") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:        
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter sample-rate"));
            return;
        }

        session->parameters.sample_rate = x;
        return;
    }

    if (strcmp(name, "scheduler-interval") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter scheduler-interval"));
            return;
        }

        session->parameters.scheduler_interval = x;
        return;
    }

    if (strcmp(name, "latency") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter latency"));
            return;
        }

        session->parameters.latency_sh[0] = x;
        session->parameters.latency_sh[1] = x;
        return;
    }
    
    if (strcmp(name, "latency-exclusive") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter latency-exclusive"));
            return;
        }

        session->parameters.latency_ex[0] = x;
        session->parameters.latency_ex[1] = x;
        return;
    }

    if (strcmp(name, "input-latency") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter input-latency"));
            return;
        }

        session->parameters.latency_sh[0] = x;
        return;
    }
    
    if (strcmp(name, "input-latency-exclusive") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter input-latency-exclusive"));
            return;
        }

        session->parameters.latency_ex[0] = x;
        return;
    }

    if (strcmp(name, "output-latency") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter output-latency"));
            return;
        }

        session->parameters.latency_sh[1] = x;
        return;
    }
    
    if (strcmp(name, "output-latency-exclusive") == 0) {
        double x;

        switch (fa_dynamic_get_type(value)) {
            case i32_type_repr:
            case f32_type_repr:
            case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter output-latency-exclusive"));
            return;
        }

        session->parameters.latency_ex[1] = x;
        return;
    }

    if (strcmp(name, "vector-size") == 0) {
        int x;

        switch (fa_dynamic_get_type(value)) {
        case i8_type_repr:
        case i16_type_repr:
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default: {
                fa_string_t type_name = fa_type_name_of(value);
                fa_slog_warning("Wrong type for set_parameter vector-size: ", value, type_name);
                fa_destroy(type_name);
                return;
            }
        }

        if (x <= kMaxVectorSize) {
            session->parameters.vector_size_sh = x;
        } else {
            fa_warn(fa_string_format_integral("vector-size %d too large, ignoring parameter.", x));
        }
        return;
    }
    
    if (strcmp(name, "vector-size-exclusive") == 0) {
        int x;

        switch (fa_dynamic_get_type(value)) {
        case i8_type_repr:
        case i16_type_repr:
        case i32_type_repr:
        case f32_type_repr:
        case f64_type_repr:
            x = fa_peek_number(value);
            break;

        default: {
                fa_string_t type_name = fa_type_name_of(value);
                fa_slog_warning("Wrong type for set_parameter vector-size-exclusive: ", value, type_name);
                fa_destroy(type_name);
                return;
            }
        }

        if (x <= kMaxVectorSize) {
            session->parameters.vector_size_ex = x;
        } else {
            fa_warn(fa_string_format_integral("vector-size-exclusive %d too large, ignoring parameter.", x));
        }
        return;
    }

    if (strcmp(name, "exclusive") == 0) {
        int64_t x;

        switch (fa_dynamic_get_type(value)) {
        case i8_type_repr:
        case i16_type_repr:
        case i32_type_repr:
            x = fa_peek_integer(value);
            break;

        case i64_type_repr:
            x = fa_peek_int64(value);
            break;

        case bool_type_repr:
            x = fa_to_bool(value);
            break;

        default:
            fa_warn(fa_string("Wrong type for set_parameter exclusive"));
            return;
        }
        
        switch(x) {
        case 0: session->parameters.exclusive = em_never; break;
        case 1: session->parameters.exclusive = em_always; break;
        case 2: session->parameters.exclusive = em_try; break;
        default: fa_warn(fa_string_format_integral("Exclusive mode %d unknown, ignoring parameter.", x)); break;
        }

        return;
    }
    fa_warn(fa_dappend(fa_string("Unknown setting: "), fa_string(name)));
}

void fa_audio_set_parameter(fa_string_t name,
                            fa_ptr_t value,
                            session_t session)
{
    char* param = fa_unstring(name);
    set_parameter(param, value, session);
    fa_free(param);
    fa_destroy(name);
    fa_destroy(value);
}

fa_list_t fa_audio_current_sessions()
{
    if (!current_session) {
        return fa_empty();
    } else {
        return list(current_session);
    }
}

fa_ptr_t fa_audio_end_all_sessions()
{
    fa_dfor_each(x, fa_audio_current_sessions()) {
        fa_audio_end_session(x);
    }
    return NULL;
}

fa_list_t fa_audio_all(session_t session)
{
    return fa_copy(session->devices);
}

// fa_list_t fa_audio_hosts()
// {
//     PaHostApiIndex count = Pa_GetHostApiCount();
//     PaHostApiIndex default_api = Pa_GetDefaultHostApi();
//     fa_list_t result = fa_list_empty();
//     for (PaHostApiIndex i = 0; i < count; i++) {
//         const PaHostApiInfo *info = Pa_GetHostApiInfo(i);
//         fa_map_t map = fa_map_empty();
//         fa_map_set_value_destructor(map, fa_destroy);
//         map = fa_map_dset(fa_string("type"), fa_i16(info->type), map);
//         map = fa_map_dset(fa_string("name"), fa_string(info->name), map);
//         map = fa_map_dset(fa_string("deviceCount"), fa_i16(info->deviceCount), map);
//         if (i == default_api) {
//             map = fa_map_dset(fa_string("default"), fa_from_bool(true), map);
//         }
//         fa_push_list(map, result);
//     }
//     return fa_list_dreverse(result);
// }

fa_list_t fa_audio_host_names(session_t session)
{
    return fa_deep_copy(session->host_names);
}

#define fail_if_no_input(type) \
    if (!session->def_input) { \
        return (type) audio_device_error(fa_string("No input device available")); \
    }
#define fail_if_no_output(type) \
    if (!session->def_output) { \
        return (type) audio_device_error(fa_string("No output device available")); \
    }

fa_pair_t fa_audio_default(session_t session)
{
    fail_if_no_input(fa_pair_t);
    fail_if_no_output(fa_pair_t);
    return fa_pair_create(session->def_input, session->def_output);
}

device_t fa_audio_default_input(session_t session)
{
    fail_if_no_input(device_t);
    return session->def_input;
}

device_t fa_audio_default_output(session_t session)
{
    fail_if_no_output(device_t);
    return session->def_output;
}

session_t fa_audio_session(device_t device)
{
    return device->session;
}

fa_string_t fa_audio_name(device_t device)
{
    return fa_copy(device->name);
}

fa_string_t fa_audio_host_name(device_t device)
{
    return fa_copy(device->host_name);
}

fa_string_t fa_audio_full_name(device_t device)
{
    fa_string_t str = fa_string("");
    fa_dwrite_string(str, fa_audio_host_name(device));
    fa_dwrite_string(str, fa_string(" "));
    fa_dwrite_string(str, fa_string_format_integral("[%d/", fa_audio_input_channels(device)));
    fa_dwrite_string(str, fa_string_format_integral("%d]", fa_audio_output_channels(device)));
    fa_dwrite_string(str, fa_string(" "));
    fa_dwrite_string(str, fa_audio_name(device));
    return str;
}

int fa_audio_input_channels(device_t device)
{
    const PaDeviceInfo *info = Pa_GetDeviceInfo(device->index);
    return info->maxInputChannels;
}

int fa_audio_output_channels(device_t device)
{
    const PaDeviceInfo *info = Pa_GetDeviceInfo(device->index);
    return info->maxOutputChannels;
}

bool fa_audio_has_input(device_t device)
{
    return fa_audio_input_channels(device) != 0;
}
bool fa_audio_has_output(device_t device)
{
    return fa_audio_output_channels(device) != 0;
}


// TODO add/remove session status listener


void fa_audio_add_status_callback(status_callback_t function,
                                  fa_ptr_t             data,
                                  session_t         session)
{
    int n = session->callbacks.count++;
    assert(n < kMaxStatusCallbacks);

    session->callbacks.elements[n].function = function;
    session->callbacks.elements[n].data     = data;
}

double fa_audio_stream_sample_rate(fa_audio_stream_t stream)
{
    return stream->sample_rate;
}

double fa_audio_default_sample_rate(fa_audio_device_t device)
{
    const PaDeviceInfo *info = Pa_GetDeviceInfo(device->index);
    return info->defaultSampleRate;
}

fa_pair_t fa_audio_recommended_latency(fa_audio_device_t device)
{
    const PaDeviceInfo *info = Pa_GetDeviceInfo(device->index);
    return fa_pair_create(fa_pair_create(
                              fa_f64(info->defaultLowInputLatency),
                              fa_f64(info->defaultHighInputLatency)
                          ), fa_pair_create(
                              fa_f64(info->defaultLowOutputLatency),
                              fa_f64(info->defaultHighOutputLatency)
                          ));
}



// --------------------------------------------------------------------------------
// TODO: fix memory leak originating from the recursive nature of the range pair

inline static
fa_string_t dshow_range(fa_pair_t x)
{
    fa_string_t str = fa_string("");
    fa_unpair(x, a, b) {
        fa_dwrite_string(str, fa_string("("));
        fa_dwrite_string(str, fa_string_show(a));
        fa_dwrite_string(str, fa_string(","));
        fa_dwrite_string(str, fa_string_show(b));
        fa_dwrite_string(str, fa_string(")"));
		fa_destroy(a);
		fa_destroy(b);
    }
	fa_destroy(x);
    return str;
}

static bool is_wasapi_device(device_t device)
{
    if (device) {
        // Note that device->host is a runtime-determined index,
        // so we need toook up static type id here
        if (Pa_GetHostApiInfo(device->host)->type == paWASAPI) {
            return true;
        }
    }

    return false;
}

inline static
void print_audio_info(device_t input, device_t output)
{
    fa_inform(fa_string("Opening real-time audio stream"));
    fa_inform(fa_string_dappend(fa_string("    Input: "), input ? fa_audio_full_name(input) : fa_string("(none)")));

    if (input) {
        fa_inform(fa_string_dappend(fa_string("        Default Latency:      "),
									dshow_range(fa_pair_first(fa_audio_recommended_latency(input)))));
        fa_inform(fa_string_format_floating("        Default Sample Rate:  %2f", fa_audio_default_sample_rate(input)));
    }

    fa_inform(fa_string_dappend(fa_string("    Output: "), output ? fa_audio_full_name(output) : fa_string("(none)")));

    if (output) {
        fa_inform(fa_string_dappend(fa_string("        Default Latency:      "),
									dshow_range(fa_pair_first(fa_audio_recommended_latency(output)))));
        fa_inform(fa_string_format_floating("        Default Sample Rate:  %2f", fa_audio_default_sample_rate(output)));
    }

    fa_let(session, input ? input->session : output->session) {
        if (session->parameters.sample_rate == 0) {
            fa_inform(fa_string("    Suggested Sample Rate:    [Use Output Default]"));
        } else {
            fa_inform(fa_string_format_floating("    Suggested Sample Rate:    %2f", session->parameters.sample_rate));
        }

        if (is_wasapi_device(input) || is_wasapi_device(output)) {
            if (session->parameters.exclusive == em_never) {
                fa_inform(fa_string("    Exclusive Mode:           No"));
                fa_inform(fa_string_format_floating("    Suggested Input Latency:  %3f", session->parameters.latency_sh[0]));
                fa_inform(fa_string_format_floating("    Suggested Output Latency: %3f", session->parameters.latency_sh[1]));
                fa_inform(fa_string_format_integral("    Vector Size:              %d",  session->parameters.vector_size_sh));
            } else if (session->parameters.exclusive == em_always) {
                fa_inform(fa_string("    Exclusive Mode:           Yes"));
                fa_inform(fa_string_format_floating("    Suggested Input Latency:  %3f", session->parameters.latency_ex[0]));
                fa_inform(fa_string_format_floating("    Suggested Output Latency: %3f", session->parameters.latency_ex[1]));
                fa_inform(fa_string_format_integral("    Vector Size:              %d",  session->parameters.vector_size_ex));
            } else {
                fa_inform(fa_string("    Exclusive Mode:           Try"));
                fa_inform(fa_string_dappend(
                    fa_string_format_floating("    Suggested Input Latency:  %3f", session->parameters.latency_ex[0]),
                    fa_string_format_floating(" / %3f", session->parameters.latency_sh[0])));
                fa_inform(fa_string_dappend(
                    fa_string_format_floating("    Suggested Output Latency: %3f", session->parameters.latency_ex[1]),
                    fa_string_format_floating(" / %3f", session->parameters.latency_sh[1])));
                fa_inform(fa_string_dappend(
                    fa_string_format_integral("    Vector Size Ex/Sh:        %d",  session->parameters.vector_size_ex),
                    fa_string_format_integral(" / %d", session->parameters.vector_size_sh)));
            }
        } else {
            fa_inform(fa_string_format_floating("    Suggested Input Latency:  %3f", session->parameters.latency_sh[0]));
            fa_inform(fa_string_format_floating("    Suggested Output Latency: %3f", session->parameters.latency_sh[1]));
            fa_inform(fa_string_format_integral("    Vector Size:              %d",  session->parameters.vector_size_sh));
        }
    }
}

// inline static
// void print_fa_signal_tree(fa_ptr_t x)
// {
//     fa_inform(fa_string_dappend(fa_string("    Signal Tree: \n"), fa_string_show(x)));
// }

inline static
fa_list_t apply_processor(proc_t proc, fa_ptr_t proc_data, fa_list_t inputs)
{
    if (proc) {
        return proc(proc_data, inputs);
    } else {
        // TODO check number of channels is < kMaxSignals
        fa_warn(fa_string("Audio.openStream: Assuming stereo output"));
        return list(fa_signal_constant(0), fa_signal_constant(0));
    }
}

int64_t audio_stream_milliseconds(fa_ptr_t a);

stream_t fa_audio_open_stream(device_t input,
                              device_t output,
                              proc_t proc,
                              fa_ptr_t proc_data
                              )
{
    return fa_audio_open_stream_with_callbacks(input, output, proc, proc_data, NULL, NULL, NULL);
}

stream_t fa_audio_open_stream_with_callbacks(device_t input,
                                             device_t output,
                                             proc_t proc,
                                             fa_ptr_t proc_data,
                                             fa_audio_message_callback_t message_callback,
                                             fa_audio_stream_closed_callback_t closed_callback,
                                             fa_ptr_t user_data)
{
    PaError         status;

    if (!input && !output) {
        return (stream_t) audio_device_error_with(
                   fa_string("Can not open a stream with no devices"), 0);
    }

    if (input && output && (input->session != output->session)) {
        return (stream_t) audio_device_error_with(
                   fa_string("Can not open a stream on devices from different sessions"), 0);
    }

    double sample_rate = (input ? input : output)->session->parameters.sample_rate;
    // sample_rate == 0 means that we are going to use the sample rate of the output device
    // (or the input device, if there is not output)
    if (sample_rate == 0) {
        sample_rate = fa_audio_default_sample_rate(output ? output : input);
    }
    stream_t stream = new_stream(input, output, sample_rate);

    {
        // TODO allow any number of inputs
        fa_list_t inputs = list(fa_signal_input(kInputOffset + 0), fa_signal_input(kInputOffset + 1));
        fa_list_t signals = apply_processor(proc, proc_data, inputs);

        stream->signal_count = fa_list_length(signals);

        for (int i = 0; i < stream->signal_count; ++i) {
            stream->signals[i] = fa_list_index(i, signals);
        }
    }
    {
        /*
            Print info messages.
         */
        print_audio_info(input, output);
    }
    fa_let(session, input ? input->session : output->session) {

        bool wasapi = (input && is_wasapi_device(input)) || (output && is_wasapi_device(output));
        bool exclusive = wasapi && session->parameters.exclusive;
        double inputLatency  = exclusive ? session->parameters.latency_ex[0] : session->parameters.latency_sh[0];
        double outputLatency = exclusive ? session->parameters.latency_ex[1] : session->parameters.latency_sh[1];
#if _WIN32
        PaWasapiFlags wasapiFlags = 0;
        wasapiFlags |= (exclusive ? paWinWasapiExclusive : 0);
        
        struct PaWasapiStreamInfo wasapiInfo = {
            .size                       = sizeof(PaWasapiStreamInfo),
            .hostApiType                = paWASAPI,
            .version                    = 1,
            .flags                      = wasapiFlags,
            .channelMask                = 0,
            .hostProcessorOutput        = NULL,
            .hostProcessorInput         = NULL,
            .threadPriority             = eThreadPriorityProAudio
        };
#else
        void *wasapiInfo = 0;
#endif

        /*
            Open and set native stream.
         */
        PaStreamParameters input_stream_parameters = {
            .suggestedLatency           = inputLatency,
            .hostApiSpecificStreamInfo  = (wasapi ? &wasapiInfo : 0),
            .device                     = (input ? input->index : 0),
            .sampleFormat               = (paFloat32 | paNonInterleaved),
            .channelCount               = stream->input_channels
        };

        PaStreamParameters output_stream_parameters = {
            .suggestedLatency           = outputLatency,
            .hostApiSpecificStreamInfo  = (wasapi ? &wasapiInfo : 0),
            .device                     = (output ? output->index : 0),
            .sampleFormat               = (paFloat32 | paNonInterleaved),
            .channelCount               = stream->output_channels
        };

        PaStreamFlags     flags    = paNoFlag;
        PaStreamCallback *callback = native_audio_callback;
        fa_ptr_t          data     = stream;

        // Before trying to open the stream, check that the format is supported
        // (But don't do that if we are prepared to retry with another format)
        // TODO: Do this also for exclusive mode, but make sure it
        //       will retry correctly even if this test fails
        if (!(exclusive && session->parameters.exclusive == em_try)) {
            status = Pa_IsFormatSupported(
                input ? &input_stream_parameters : NULL,
                output ? &output_stream_parameters : NULL,
                sample_rate
                );

            if (status != paNoError) {
                after_failed_processing(stream);
                return (stream_t) audio_device_error_with(fa_string("Stream parameters not supported"), status);
            }
        }
        
        unsigned long buffer_size = exclusive ? session->parameters.vector_size_ex : session->parameters.vector_size_sh;

        status = Pa_OpenStream(
                     &stream->native,
                     input ? &input_stream_parameters : NULL,
                     output ? &output_stream_parameters : NULL,
                     sample_rate, buffer_size, flags,
                     callback, data
                 );
                   
        // Retry in shared mode
        if ((status != paNoError) && exclusive && session->parameters.exclusive == em_try) {
            fa_slog_info("Could not open WASAPI stream in exclusive mode, trying shared mode...");
            fa_inform(fa_string((char *) Pa_GetErrorText(status)));

            exclusive = false;
            buffer_size = session->parameters.vector_size_sh;
            inputLatency = session->parameters.latency_sh[0];
            outputLatency = session->parameters.latency_sh[1];
            
#if _WIN32
            wasapiInfo.flags = 0;
#endif
            input_stream_parameters.hostApiSpecificStreamInfo = &wasapiInfo;
            output_stream_parameters.hostApiSpecificStreamInfo = &wasapiInfo;
            input_stream_parameters.suggestedLatency = inputLatency;
            output_stream_parameters.suggestedLatency = outputLatency;
            
            status = Pa_OpenStream(
                         &stream->native,
                         input ? &input_stream_parameters : NULL,
                         output ? &output_stream_parameters : NULL,
                         sample_rate, buffer_size, flags,
                         callback, data
                     );
        }
        
        if (status != paNoError) {
            after_failed_processing(stream);
            return (stream_t) audio_device_error_with(fa_string("Could not open stream"), status);
        }

        stream->max_buffer_size = buffer_size;
        stream->exclusive_mode  = exclusive;

        status = Pa_SetStreamFinishedCallback(stream->native, native_finished_callback);
        
        if (status != paNoError) {
            after_failed_processing(stream);
            return (stream_t) audio_device_error_with(fa_string("Could not set stream finished callback"), status);
        }
    
        fa_inform(fa_string("Started stream using these values:"));
        if (wasapi) {
            fa_inform(fa_dappend(fa_string("    Exclusive Mode:           "), fa_string(exclusive ? "Yes" : "No")));
        } else {
            fa_inform(fa_string("    Exclusive Mode:           n/a"));
        }
        fa_inform(fa_string_format_floating("    Sample Rate:              %2f", sample_rate));
        fa_inform(fa_dappend(fa_string("    Suggested Input Latency:  "),
            input ? fa_string_format_floating("%3f", inputLatency) : fa_string("n/a")));
        fa_inform(fa_dappend(fa_string("    Suggested Output Latency: "),
            output ? fa_string_format_floating("%3f", outputLatency) : fa_string("n/a")));
        fa_inform(fa_string_format_integral("    Vector Size:              %d",  buffer_size));
    
    }
    
    // Add callbacks
    if (message_callback) {
        fa_audio_add_message_callback(message_callback, user_data, stream);
    }
    if (closed_callback) {
        stream->closed_callback = pair(closed_callback, user_data);
    }
    
    {
        /*
            Prepare and launch DSP thread.
         */
        before_processing(stream);

        status = Pa_StartStream(stream->native);
        
        //printf("Time in beginning: %lld\n", audio_stream_milliseconds(stream));
        //stream->start_time = audio_stream_milliseconds(stream);
        //printf("Setting start-time to %lld in beginning\n", stream->start_time);

        if (status != paNoError) {
            // TODO is finished callback invoked here
            // after_failed_processing(stream);
            return (stream_t) audio_device_error_with(fa_string("Could not start stream"), status);
        }
        
        //stream->start_time = audio_stream_milliseconds(stream);
    }
    {
        /*
            Launch control thread.
         */
        fa_ptr_t audio_control_thread(fa_ptr_t data);
        stream->controller.stop   = false;
        stream->controller.thread = fa_thread_create(audio_control_thread, stream, fa_string("Audio controller thread"));
        // stream->controller.mutex  = fa_thread_create_mutex();
    }


    fa_let(session, input ? input->session : output->session) {
        fa_push_list(stream, session->streams);
    }
    return stream;
}

void fa_audio_close_stream(stream_t stream)
{
    fa_inform(fa_string("Closing real-time audio stream"));
    // fa_inform(fa_string_format_integral("  Stream: %p \n", (long) stream));

    {
        // TODO need atomic
        native_stream_t native = stream->native;

        if (native) {
            fa_inform(fa_string("    Closing native stream"));
            stream->native = NULL;

            PaError error;

            if ((error = Pa_StopStream(native)) != paNoError) {
                fa_warn(fa_string("Could not stop stream: "));
                fa_warn(fa_string((char *) Pa_GetErrorText(error)));
            }

            if ((error = Pa_CloseStream(native)) != paNoError) {
                fa_warn(fa_string("Could not close stream"));
                fa_warn(fa_string((char *) Pa_GetErrorText(error)));
            }

            // Note that Pa_StopStream blocks until the stream is stopped,
            // so after_processing should be guaranteed to have run at this point.

            fa_inform(fa_string("    Native stream closed"));
            fa_inform(fa_string("    Stopping stream controller"));

            stream->controller.stop = true;
            fa_thread_join(stream->controller.thread);
            // fa_thread_destroy_mutex(stream->controller.mutex);

            fa_inform(fa_string("    Stream controller thread stopped"));
        }
    }

    // Not deleted until sesion is gone
}

void fa_audio_with_stream(device_t            input,
                          device_t            output,
                          proc_t              proc,
                          fa_ptr_t               proc_data,
                          stream_callback_t   stream_callback,
                          fa_ptr_t               stream_data,
                          fa_error_callback_t    error_callback,
                          fa_ptr_t               error_data)
{
    stream_t stream = fa_audio_open_stream(input, output, proc, proc_data);

    if (fa_check(stream)) {
        error_callback(error_data, (fa_error_t) stream);
    } else {
        stream_callback(stream_data, stream);
    }

    fa_audio_close_stream(stream);
}

fa_list_t fa_audio_devices(fa_audio_stream_t stream)
{
    if (fa_equal(stream->input, stream->output)) {
        return list(stream->input);
    } else {
        return list(stream->input, stream->output);
    }
}

fa_clock_t fa_audio_stream_clock(fa_audio_stream_t stream)
{
    return (fa_clock_t) stream;
}

fa_clock_t fa_audio_get_clock(fa_audio_stream_t stream)
{
    return (fa_clock_t) stream;
}

void fa_audio_set_speed(double speed, fa_audio_stream_t stream)
{
    if (stream->state) {
        state_base_t state = (state_base_t) stream->state;
        state->speed = speed;
    }
}

void fa_audio_add_message_callback(fa_audio_message_callback_t function,
                                   fa_ptr_t data,
                                   fa_audio_stream_t stream)
{
    int n = stream->callbacks.count++;
    assert(n < kMaxMessageCallbacks);
    stream->callbacks.elements[n].function = function;
    stream->callbacks.elements[n].data     = data;
}

void fa_audio_schedule(fa_time_t time,
                       fa_action_t action,
                       fa_audio_stream_t stream)
{
    fa_pair_left_t pair = fa_pair_left_create(time, action);

    fa_atomic_queue_write(stream->before_controls, pair);
}

void fa_audio_schedule_relative(fa_time_t         time,
                                fa_action_t       action,
                                fa_audio_stream_t stream)
{
    fa_time_t now = fa_clock_time(fa_audio_stream_clock(stream));
    fa_audio_schedule(fa_dadd(now, time), action, stream);
}

void fa_audio_schedule_now(fa_action_t action, fa_audio_stream_t stream)
{
    if (fa_action_is_flat(action)) {
        fa_list_t actions = fa_action_flat_to_list(action);
        fa_for_each(a, actions) {
            fa_atomic_queue_write(stream->short_controls, a);
        }
        fa_destroy(actions);
    } else {
        fa_warn(fa_string("Non-flat action passed to fa_audio_schedule_now"));
    }
}

fa_map_t fa_audio_stream_get_info(fa_audio_stream_t stream)
{
    const PaStreamInfo *streamInfo = Pa_GetStreamInfo(stream->native);
    //printf("Actual input latency: %f\n", streamInfo->inputLatency);
    //printf("Actual output latency: %f\n", streamInfo->outputLatency);
    fa_map_t map = fa_map_empty();
    fa_map_set_value_destructor(map, fa_destroy);
    map = fa_map_dadd(fa_string("inputLatency"), fa_from_double(streamInfo->inputLatency), map);
    map = fa_map_dadd(fa_string("outputLatency"), fa_from_double(streamInfo->outputLatency), map);
    map = fa_map_dadd(fa_string("sampleRate"), fa_from_double(streamInfo->sampleRate), map);
    map = fa_map_dadd(fa_string("roundtripLatency"), fa_from_double(stream->roundtrip_latency), map);
    map = fa_map_dadd(fa_string("exclusive"), fa_from_bool(stream->exclusive_mode), map);
    return map;
}


fa_ptr_t forward_action_to_audio_thread(fa_ptr_t x, fa_ptr_t action, fa_ptr_t t)
{
    stream_t stream = x;
    fa_time_t time = t; // This is the nominal time for the action, i.e. the time it was scheduled at
    fa_action_timestamp_set(action, fa_time_to_double(time));
    fa_atomic_queue_write(stream->in_controls, action);
    return NULL;
}
fa_ptr_t audio_control_thread(fa_ptr_t x)
{
    stream_t stream = x;

    fa_inform(fa_string("    Audio control thread active"));

    //printf("Audio control thread active %p\n", pthread_self());

    while (true) {
        if (stream->controller.stop) {
            break;
        }
        
        // ////
        // state_base_t state = (state_base_t) stream->state;
        // printf("Time: %u  Count: %u  Total: %u\n", fa_clock_milliseconds(fa_audio_stream_clock(stream)), state->count);
        // ////

        {
            fa_ptr_t nameValue;

            while ((nameValue = fa_atomic_queue_read(stream->out_controls))) {
                int n = stream->callbacks.count;


                fa_unpair(nameValue, name, value) {

                    // FIXME assure that this copying can not happen after stream has been
                    // stopped
                    fa_string_t name2 = fa_copy(name);
                    fa_ptr_t   value2 = fa_copy(value);
                    // fa_inform(fa_string_show(fa_pair_create(name2, value2)));

                    for (int j = 0; j < n; ++j) {
                        fa_binary_t cbFunc = stream->callbacks.elements[j].function;
                        fa_ptr_t    cbData = stream->callbacks.elements[j].data;
                        cbFunc(cbData, name2, value2);
                    }

                    fa_destroy(name2);
                    fa_destroy(value2);
                }
            }
        }

        // fa_with_lock(stream->controller.mutex)
        {
            fa_time_t now = fa_clock_time(fa_audio_stream_clock(stream));
            //fa_time_t now = fa_clock_time(fa_clock_standard());
            
            // Write incoming actions
            // TODO get things from before_controls to stream->controls
            {
                fa_ptr_t incomingPair;

                while ((incomingPair = fa_atomic_queue_read(stream->before_controls))) {
                    assert(incomingPair);
                    fa_priority_queue_insert(incomingPair, stream->controls);
                }
            }

            run_actions(stream->controls,
                        now,
                        forward_action_to_audio_thread,
                        stream
                       );
            fa_destroy(now);

            // Note: thread_sleep may be to inexact.

            // We need to find a more precise way of invoking the scheduler. We could either
            //  * Look for a platform-independent timing library
            //  * Write platform-specific code
            //  * Use notifications from the audio thread (might not work at startup)

            // fa_thread_sleep(1);
            // printf("-- waking up at %d\n", fa_clock_milliseconds(fa_clock_standard()));
            fa_thread_sleep((stream->input ? stream->input : stream->output)->session->parameters.scheduler_interval);
        }
    }
    
    if (stream->closed_callback) {
        bool finished_normally = (stream->native == NULL);
        fa_audio_stream_closed_callback_t callback = fa_pair_first(stream->closed_callback);
        fa_ptr_t user_data = fa_pair_second(stream->closed_callback);
        fa_slog_info("        Calling closed_callback with data ", user_data, fa_from_bool(finished_normally));
        stream->closed_callback = NULL;
        callback(user_data, finished_normally);
    }

    fa_inform(fa_string("    Audio control thread finished"));
    return NULL;
}

// --------------------------------------------------------------------------------

#define VALS inputs
#define MERGED_SIGNAL signals[0]


void before_processing(stream_t stream)
{
    //session_t session  = stream->input ? stream->input->session : stream->output->session;
    stream->state      = new_state(stream->sample_rate);

    fa_signal_t merged = fa_signal_constant(0);

    for (int c = 0; c < stream->signal_count; ++c) {
        fa_signal_t withOutput = fa_signal_output(0, kOutputOffset + c, stream->signals[c]);
        merged = fa_signal_former(merged, withOutput); // Could use any combinator here
    }

    fa_list_t procs = fa_signal_get_procs(merged);

    fa_inform(fa_string_format_integral("    Custom processors: %d", fa_list_length(procs)));

    fa_map_t proc_map = build_proc_map(procs);
    fa_inform(fa_dappend(fa_string("        Allocated channel offsets: "), fa_string_show(proc_map)));

    fa_for_each(x, procs) {
        // printf("Adding custom proc %p!\n", x);
        add_custom_proc(x, stream->state);
    }

    merged = fa_signal_simplify(merged);
    //print_fa_signal_tree(merged);

    merged = fa_signal_route_processors(proc_map, merged);
    //print_fa_signal_tree(merged);

    merged = fa_signal_doptimize(merged);
    merged = fa_signal_dverify(merged);

    stream->MERGED_SIGNAL = merged;
    run_custom_procs(custom_proc_before, 0, stream->state);
}

void after_processing(stream_t stream)
{
    fa_inform(fa_string("        Stream finished normally, destroying external processors."));
    run_custom_procs(custom_proc_after, 0, stream->state);
    run_custom_procs(custom_proc_destroy, 0, stream->state);

    delete_state(stream->state);
}

void after_failed_processing(stream_t stream)
{
    // This is ugly: create a temporary state just to enumerate processors
    fa_inform(fa_string("        Streams did not start, destroying external processors."));
    session_t session  = stream->input ? stream->input->session : stream->output->session;
    stream->state      = new_state(session->parameters.sample_rate); // FIXME

    fa_signal_t merged = fa_signal_constant(0);

    for (int c = 0; c < stream->signal_count; ++c) {
        fa_signal_t withOutput = fa_signal_output(0, kOutputOffset + c, stream->signals[c]);
        merged = fa_signal_former(merged, withOutput); // Could use any combinator here
    }

    fa_for_each(x, fa_signal_get_procs(merged)) {
        // printf("Adding custom proc %p!\n", x);
        add_custom_proc(x, stream->state);
    }
    run_custom_procs(custom_proc_destroy, 0, stream->state);
    delete_state(stream->state);
}

fa_ptr_t run_simple_action2(fa_ptr_t x, fa_ptr_t a)
{
    return run_simple_action(x, a);
}
void handle_outgoing_message(fa_ptr_t x, fa_string_t name, fa_ptr_t value)
{
    stream_t stream = x;
    fa_mark_used(stream);
    fa_atomic_queue_write(stream->out_controls, fa_pair_create(name, value));
}

void during_processing(stream_t stream, unsigned count, double time, float **input, float **output)
{
    state_base_t state = (state_base_t) stream->state;
    
    // if (!stream->start_time) {
    //     stream->start_time = audio_stream_milliseconds(stream);
    //     printf("setting start-time to %lld\n", stream->start_time);
    // }
    //   
    // {
    //     fa_ptr_t action;
    //
    //     while ((action = fa_atomic_queue_read(stream->in_controls))) {
    //         run_simple_action2(stream->state, action);
    //     }
    // }
    {
        fa_ptr_t action;

        while ((action = fa_atomic_queue_read(stream->short_controls))) {
            run_simple_action2(stream->state, action);
        }
    }

    // Outgoing controls
    {
        custom_procs_receive((state_t) state, handle_outgoing_message, stream);
    }

    if (!kVectorMode) {
        //printf("during_processing %u\n", count);

        fa_ptr_t next_action = NULL;
        double time_per_frame = (double) 1.0 / (double) stream->sample_rate;
        for (int i = 0; i < count; ++ i) {
            {
                double frame_time = time + (time_per_frame * i);
                while (1) {
                    next_action = fa_atomic_queue_peek(stream->in_controls);
                    //if (next_action) {
                    //    printf("timestamp: %llu   count: %llu\n", fa_action_timestamp(next_action), state->count);
                    //}
                    if (next_action && fa_action_timestamp(next_action) <= frame_time) { //state->count) {
                        next_action = fa_atomic_queue_read(stream->in_controls);
                        run_simple_action2(stream->state, next_action);
                    } else {
                        break;
                    }
                }
            }
                        
            run_custom_procs(custom_proc_render, count, stream->state);

            if (stream->input) {
                for (int c = 0; c < stream->input_channels; ++c) {
                    state->VALS[(c + kInputOffset) * kMaxVectorSize] = input[c][i];
                }
            }

            step(stream->MERGED_SIGNAL, stream->state);

            if (stream->output) {
                for (int c = 0; c < stream->output_channels; ++c) {
                    output[c][i] = state->VALS[(c + kOutputOffset) * kMaxVectorSize];
                }
            }

            inc_state1(stream->state);
        }
    } else {
        // assert((count == kMaxVectorSize) && "Wrong vector size");
        // assert((stream->signal_count == 2) && "Wrong number of channels");

        if (stream->input) {
            for (int i = 0; i < count; ++ i) {
                for (int c = 0; c < stream->input_channels; ++c) {
                    state->VALS[(c + kInputOffset) * kMaxVectorSize + i] = input[c][i];
                }
            }
        }

        {
            double dummy_output[count];
            run_custom_procs(custom_proc_render, count, stream->state);
            step_vector(stream->MERGED_SIGNAL, stream->state, count, dummy_output);
        }

        inc_state(count, stream->state);

        if (stream->output) {
            for (int i = 0; i < count; ++ i) {
                for (int c = 0; c < stream->output_channels; ++c) {
                    output[c][i] = state->VALS[(c + kOutputOffset) * kMaxVectorSize + i];
                }
            }
        }
    }
}


/*  The callbacks

    We want to assume that this happens in this order fro any stream:
        1) during_processing is called n times
        2) after_processing is called once

   With the CoreAudio backend, PortAudio sometimes violates this by calling
   during_processing after after_processing, or calling after_processing
   more than once. We guard against this by setting the stream state to NULL.
*/

int native_audio_callback(const void                       *input,
                          void                             *output,
                          unsigned long                     count,
                          const PaStreamCallbackTimeInfo   *time_info,
                          PaStreamCallbackFlags             flags,
                          void                             *data)
{
    stream_t stream = data;

    if (stream->state) {
        
        // Cache "actual" latency
        stream->roundtrip_latency = time_info->outputBufferDacTime - time_info->inputBufferAdcTime;
        
        during_processing(stream, count, time_info->outputBufferDacTime, (float **) input, (float **) output);
        stream->pa_flags |= flags;
    }

    return paContinue;
    // else {
    //     return paAbort;
    // }
}

void native_finished_callback(void *data)
{
    stream_t stream = data;

    if (stream->state) {

        if (stream->pa_flags & paInputOverflow) {
            fa_warn(fa_string("    Input overflow detected"));
        }

        if (stream->pa_flags & paOutputUnderflow) {
            fa_warn(fa_string("    Output underflow detected"));
        }

        // fa_inform(fa_string_format_integral("    Stream flag result (0 = ok): %d", stream->pa_flags));
        after_processing(data);

        stream->state = NULL;
    }
    
    if (stream->native) {
        stream->controller.stop = true;
    }
}


// --------------------------------------------------------------------------------

bool audio_session_equal(fa_ptr_t a, fa_ptr_t b)
{
    return a == b;
}

fa_string_t audio_session_show(fa_ptr_t a)
{
    fa_string_t str = fa_string("<AudioSession ");
    str = fa_string_dappend(str, fa_string_format_integral("%p", (long) a));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

void audio_session_destroy(fa_ptr_t a)
{
    fa_audio_end_session(a);
}

fa_ptr_t audio_session_impl(fa_id_t interface)
{
    static fa_equal_t audio_session_equal_impl
        = { audio_session_equal };
    static fa_string_show_t audio_session_show_impl
        = { audio_session_show };
    static fa_destroy_t audio_session_destroy_impl
        = { audio_session_destroy };

    switch (interface) {
    case fa_equal_i:
        return &audio_session_equal_impl;

    case fa_string_show_i:
        return &audio_session_show_impl;

    case fa_destroy_i:
        return &audio_session_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool audio_device_equal(fa_ptr_t a, fa_ptr_t b)
{
    device_t device1 = (device_t) a;
    device_t device2 = (device_t) b;
    // TODO check that session is valid
    return device1->index == device2->index;
}

fa_string_t audio_device_show(fa_ptr_t a)
{
    device_t device = (device_t) a;

    fa_string_t str = fa_string("<AudioDevice ");
    str = fa_string_dappend(str, fa_audio_host_name(device));
    str = fa_string_dappend(str, fa_string(" "));
    str = fa_string_dappend(str, fa_audio_name(device));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

void audio_device_destroy(fa_ptr_t a)
{
    
}

fa_dynamic_type_repr_t audio_device_get_type(fa_ptr_t a)
{
    return audio_device_type_repr;
}

fa_ptr_t audio_device_impl(fa_id_t interface)
{
    static fa_equal_t audio_device_equal_impl
        = { audio_device_equal };
    static fa_string_show_t audio_device_show_impl
        = { audio_device_show };
    static fa_destroy_t audio_device_destroy_impl
        = { audio_device_destroy };
    static fa_dynamic_t audio_device_dynamic_impl
        = { audio_device_get_type };

    switch (interface) {
    case fa_equal_i:
        return &audio_device_equal_impl;

    case fa_string_show_i:
        return &audio_device_show_impl;
        
    case fa_destroy_i:
        return &audio_device_destroy_impl;
        
    case fa_dynamic_i:
        return &audio_device_dynamic_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool audio_stream_equal(fa_ptr_t a, fa_ptr_t b)
{
    return a == b;
}

fa_string_t audio_stream_show(fa_ptr_t a)
{
    fa_string_t str = fa_string("<AudioStream ");
    str = fa_string_dappend(str, fa_string_format_integral(" %p", (long) a));
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

void audio_stream_destroy(fa_ptr_t a)
{
    fa_audio_close_stream(a);
}

int64_t audio_stream_milliseconds(fa_ptr_t a)
{
    stream_t stream = (stream_t) a;
    
    // if (stream->state) {
   //      state_base_t state = (state_base_t) stream->state;
   //      return ((double) state->count / (double) state->rate * 1000.0);
   //  }
    
    
    if (!stream->native) return 0;
    return Pa_GetStreamTime(stream->native) * 1000.0;

    // if (stream->state) {
//         // We cache time in the stream in case the stream state has been freed
//         state_base_t state = (state_base_t) stream->state;
//
// #ifdef kAllowVirtualTime
//         stream->last_time = ((double) state->elapsed_time * 1000.0);
// #else
//         stream->last_time = ((double) state->count / (double) state->rate * 1000.0);
// #endif
//     }
//
//     return stream->last_time;
}

fa_time_t fa_audio_stream_time(fa_ptr_t a)
{
    // int64_t ms = audio_stream_milliseconds(a);
    // return fa_milliseconds(ms);
    stream_t stream = (stream_t) a;
    return fa_time_from_double(Pa_GetStreamTime(stream->native));
}

fa_dynamic_type_repr_t audio_stream_get_type(fa_ptr_t a)
{
    return audio_stream_type_repr;
}

fa_ptr_t audio_stream_impl(fa_id_t interface)
{
    static fa_string_show_t audio_stream_show_impl
        = { audio_stream_show };
    static fa_clock_interface_t audio_stream_clock_impl
        = { fa_audio_stream_time, audio_stream_milliseconds };
    static fa_equal_t audio_stream_equal_impl
        = { audio_stream_equal };
    static fa_destroy_t audio_stream_destroy_impl
        = { audio_stream_destroy };
    static fa_dynamic_t audio_stream_dynamic_impl
        = { audio_stream_get_type };

    switch (interface) {

    case fa_clock_interface_i:
        return &audio_stream_clock_impl;

    case fa_string_show_i:
        return &audio_stream_show_impl;

    case fa_equal_i:
        return &audio_stream_equal_impl;

    case fa_destroy_i:
        return &audio_stream_destroy_impl;
        
    case fa_dynamic_i:
        return &audio_stream_dynamic_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

void fa_log_error_from(fa_string_t msg, fa_string_t origin);

fa_error_t audio_device_error(fa_string_t msg)
{
    fa_error_t err = fa_error_create_simple(error,
                                            msg,
                                            fa_string("Doremir.Device.Audio"));
    fa_error_log(NULL, fa_copy(err));
    return err;
}

fa_error_t audio_device_error_with(fa_string_t msg, int code)
{
    fa_string_t pa_error_str;
    if (code == 0) {
        pa_error_str = fa_string("");
    } else if (code == paUnanticipatedHostError) {
        const PaHostErrorInfo* herr = Pa_GetLastHostErrorInfo();
        if (herr) {
            pa_error_str = fa_dappend(
                fa_format_integral("Unanticipated Host Error, number: %ld, text: ", herr->errorCode),
                fa_string((char*)herr->errorText));
        } else {
            pa_error_str = fa_string("Unanticipated Host Error, Pa_GetLastHostErrorInfo() failed!");
        }
    } else {
        pa_error_str = fa_string((char *) Pa_GetErrorText(code));
    }

    fa_error_t err = fa_error_create_simple(error,
                                            fa_string_dappend(msg,
                                                              fa_string_dappend(fa_string(": "), pa_error_str)
                                                              // format_integral(" (error code %d)", code)
                                                             ),
                                            fa_string("Doremir.Device.Audio"));
    fa_error_log(NULL, fa_copy(err));
    return err;
}

void audio_device_fatal(fa_string_t msg, int code)
{
    fa_log_error_from(
        fa_string_dappend(msg, fa_format_integral(" (error code %d)", code)),
        fa_string("Doremir.Device.Audio"));

    fa_log_error(fa_string("Terminating Fa"));
    exit(error);
}


// nothing really
