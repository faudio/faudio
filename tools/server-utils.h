#include "fa/fa.h"

#ifndef __SERVER_UTILS
#define __SERVER_UTILS

#include "server-types.h"
#include "server-globals.h"
#include "fa/signal.h"

#include <string.h>

// int lo_message_add_varargs_internal(lo_message m, const char *types,
//                                     va_list ap, const char *file,
//                                     int line); // message.c
//
// int respond(lo_message message, lo_server server, const char *path, const char *types, ...)
// {
//   lo_address address = lo_message_get_source(message);
//   //const char *host = lo_address_get_hostname(address);
//   //const char *port = lo_address_get_port(address);
//
//   if (!address) {
//       printf("Couldn't get message source!\n");
//       return 0;
//   }
//
//   va_list ap;
//   va_start(ap, types);
//   int r = lo_send_from_varargs_internal(address, server, LO_TT_IMMEDIATE, path, types, ap);
//   if (r < 0)
//       printf("Error sending back message, socket may have closed.\n");
//   //else
//   //    printf("Sent %d bytes to %s:%s.\n", r, host, port);
//
//   return r;
// }

#define sched_delay fa_milliseconds(200)

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

uint8_t min_uint8(uint8_t a, uint8_t b) { return a < b ? a : b; }

#define send_osc(m, s, p, ...) \
    fa_with_lock(osc_mutex) { \
        int r = lo_send_from(lo_message_get_source(m), (lo_server)s, LO_TT_IMMEDIATE, p, __VA_ARGS__); \
        if (r < 0) fa_fail(fa_format("Could not send osc message %s: %d", p, lo_address_errno(lo_message_get_source(m)))); \
    } \

#define send_osc_async(p, ...) if (last_address) { \
    fa_with_lock(osc_mutex) { \
        int r = lo_send_from(last_address, server, LO_TT_IMMEDIATE, p, __VA_ARGS__); \
        if (r < 0) { \
          fa_fail(fa_format("Could not send async osc message %s: %d", p, lo_address_errno(last_address))); \
      } \
    } \
  }

#define check_id(id, message, user_data) \
    if (id > last_used_id) {    \
        last_used_id = id;      \
    } else {                    \
    fa_fail(fa_string_dappend(fa_string_format_integral("ID %zu is lower than", id), \
                              fa_string_format_integral(" last used ID (%zu)", last_used_id))); \
    if (message && user_data) {   \
        send_osc(message, user_data, "/error", "isi", id, "bad-id", id); \
    }  \
    return 0; \
}

#define error_check(_obj, _msg)             \
if (fa_check(_obj)) {                       \
    fa_log_error(fa_string(_msg));          \
    fa_error_log(NULL, (fa_error_t) _obj);  \
    return;                                 \
}

    //fa_slog_error("Obj: ", _obj);   
    //if (_obj) fa_destroy(_obj);     


// Windows doesn't have strdup
#ifdef _WIN32
static char *strdup(const char *s) {
    char *p = malloc(strlen(s) + 1);
    if(p) { strcpy(p, s); }
    return p;
}
#endif

static inline char* strdup_or_null(char* s) {
    return (s && *s) ? strdup(s) : NULL;
}

static inline char* fa_dunstring(fa_string_t str) {
    char* result = fa_unstring(str);
    fa_destroy(str);
    return result;
}

bool string_begins_with(const char *str, const char *prefix) {
    size_t lenpre = strlen(prefix), lenstr = strlen(str);
    return lenstr < lenpre ? false : strncmp(prefix, str, lenpre) == 0;
}

void do_schedule(fa_time_t time, fa_action_t action, fa_ptr_t stream) {
    if (stream) {
        switch (fa_dynamic_get_type(stream)) {
        case audio_stream_type_repr:
            fa_audio_schedule(time, action, stream);
            break;
        case midi_stream_type_repr:
            fa_midi_schedule(time, action, stream);
            break;
        default:
            fa_slog_info("Strange stream sent to schedule: ", stream);
            assert(false && "Unknown object sent to schedule");
        }
    } else {
        fa_deep_destroy_always(action);
    }
}

void do_schedule_relative(fa_time_t time, fa_action_t action, fa_ptr_t stream) {
    if (stream) {
        switch (fa_dynamic_get_type(stream)) {
        case audio_stream_type_repr:
            fa_audio_schedule_relative(time, action, stream);
            break;
        case midi_stream_type_repr:
            fa_midi_schedule_relative(time, action, stream);
            break;
        default:
            fa_slog_info("Strange stream sent to schedule_relative: ", stream);
            assert(false && "Unknown object sent to schedule_relative");
        }
    } else {
        fa_deep_destroy_always(action);
    }
}

void do_schedule_now(fa_action_t action, fa_ptr_t stream) {
    if (stream) {
        switch (fa_dynamic_get_type(stream)) {
        case audio_stream_type_repr:
            fa_audio_schedule_now(action, stream);
            break;
        case midi_stream_type_repr:
            fa_midi_schedule_now(action, stream);
            break;
        default:
            fa_slog_info("Strange stream sent to schedule_now: ", stream);
            assert(false && "Unknown object sent to schedule_now");
        }
    } else {
        fa_deep_destroy_always(action);
    }
}

void schedule(fa_time_t time, fa_action_t action, fa_ptr_t stream) {
    
    // Scheduling on absolute times does not need to be bundled,
    // so schedule it directly
    //
    // This solves the problem of pushing a mix of absolute and
    // relative times onto the bundle_actions list
    
    // if (in_bundle) {
    //     if (bundle_stream) {
    //         assert(bundle_stream == stream && "Cannot send to different streams in same bundle");
    //     } else {
    //         bundle_stream = stream;
    //     }
    //     fa_push_list(pair(action, time), bundle_actions);
    // } else {
        do_schedule(time, action, stream);
    //}
}

void schedule_relative(fa_time_t time, fa_action_t action, fa_ptr_t stream) {
    if (in_bundle) {
        if (bundle_stream) {
            assert(bundle_stream == stream && "Cannot send to different streams in same bundle");
        } else {
            bundle_stream = stream;
        }
        fa_push_list(pair(action, time), bundle_actions);
    } else {
        do_schedule_relative(time, action, stream);
    }
}

void schedule_now(fa_action_t action, fa_ptr_t stream) {
    if (in_bundle) {
        if (bundle_stream) {
            assert(bundle_stream == stream && "Cannot send to different streams in same bundle");
        } else {
            bundle_stream = stream;
        }
        fa_push_list(pair(action, fa_now()), bundle_actions);
    } else {
        do_schedule_now(action, stream);
    }
    
}


static inline bool timetag_is_now(lo_timetag tt) {
    return (tt.sec == 0 && tt.frac == 1);
}

double timetag_to_double(lo_timetag tt) {
    if (timetag_is_now(tt)) return 0;
    return (double) tt.sec + (double) tt.frac * 0.00000000023283064365; // from timetag.c in liblo
}

fa_ptr_t create_fa_value(lo_type type, void *data) {
    
    typedef union {
        int32_t i;
        float f;
        char c;
        uint32_t nl;
    } lo_pcast32;

    typedef union {
        int64_t i;
        double f;
        uint64_t nl;
    } lo_pcast64;
    
    lo_pcast32 val32;
    lo_pcast64 val64;
    int size;
    bool bigendian = 0; // from message.c in liblo

    size = lo_arg_size(type, data);
    if (size == 4 || type == LO_BLOB) {
        if (bigendian) {
            val32.nl = lo_otoh32(*(int32_t *) data);
        } else {
            val32.nl = *(int32_t *) data;
        }
    } else if (size == 8) {
        if (bigendian) {
            val64.nl = lo_otoh64(*(int64_t *) data);
        } else {
            val64.nl = *(int64_t *) data;
        }
    }
    
    switch (type) {
    case LO_INT32:      return fa_from_int32(val32.i);
    case LO_FLOAT:      return fa_from_float(val32.f);
    case LO_STRING:     return fa_string_from_utf8((char *) data);
    //case LO_BLOB:     return fa_buffer_wrap(data + 4, val32.i, NULL, NULL); // TODO: copy memory
    case LO_BLOB:       assert(false && "creating buffers from blobs not yet supported");
    case LO_INT64:      return fa_from_int64(val64.i);
    case LO_DOUBLE:     return fa_from_double(val64.f);
    case LO_SYMBOL:     return fa_string_from_utf8((char *) data);
    case LO_CHAR:       return fa_string_single(val32.c); // Encoding beyond ascii?
    //case LO_MIDI: 
    case LO_TRUE:       return fa_from_bool(true);
    case LO_FALSE:      return fa_from_bool(false);
    case LO_NIL:        return NULL; // ?
    default:
        fa_fail(fa_string("create_fa_value: Cannot handle that type"));
        return NULL;
    }
}

lo_timetag timetag_from_double(double dtime) {
    lo_timetag tt;
    tt.sec = (uint32_t) dtime;
    tt.frac = (uint32_t) ((dtime - tt.sec) * 4294967296.);
    return tt;
}

lo_timetag timetag_from_time(fa_time_t time) {
    return timetag_from_double(fa_time_to_double(time));
}

lo_blob lo_blob_from_buffer(fa_buffer_t buffer) {
    return lo_blob_new(fa_buffer_size(buffer), fa_buffer_unsafe_address(buffer));
}

void send_fa_value_osc(lo_message message, void *user_data, char *path, char *key, fa_ptr_t value) {
    fa_dynamic_type_repr_t type = fa_dynamic_get_type(value);
    switch(type) {
    case null_type_repr:
        send_osc(message, user_data, path, "sN", key);
        break;
    case f32_type_repr:
        send_osc(message, user_data, path, "sf", key, fa_peek_float(value));
        break;
    case f64_type_repr:
        send_osc(message, user_data, path, "sd", key, fa_peek_double(value));
        break;
    case string_type_repr:
        send_osc(message, user_data, path, "ss", key, fa_unstring(value));
        break;
    case bool_type_repr:
        if (fa_peek_bool(value)) {
            send_osc(message, user_data, path, "sT", key);
        } else {
            send_osc(message, user_data, path, "sF", key);
        }
        break;
    case i8_type_repr:
        send_osc(message, user_data, path, "si", key, fa_peek_int8(value)); // can't use peek_integer as it returns 64 bit
        break;
    case i16_type_repr:
        send_osc(message, user_data, path, "si", key, fa_peek_int16(value));
        break;
    case i32_type_repr:
        send_osc(message, user_data, path, "si", key, fa_peek_int32(value));
        break;
    default:
        fa_slog_warning("Don't know how to send value via osc: ", value);
    }
}


// void schedule_to_midi_echo_stream(fa_time_t time, fa_action_t action)
// {
  // switch (selected_midi_echo) {
  //   case FA_ECHO_AUDIO:
  //     if (current_audio_stream) fa_audio_schedule(time, action, current_audio_stream);
  //     break;
  //   case FA_ECHO_DEVICE:
  //     printf("FA_ECHO_DEVICE not implemented\n");
  //     break;
  //   case FA_ECHO_NO_ECHO:
  //     break;
  // }
// }


fa_action_t main_volume_action(int ch, int vol)
{
    assert(ch >= 0 && ch <= 0x0F && "Channel must be between 0 and 15!");
    if (vol < 0) vol = 0;
    else if (vol > 0x3FFF) vol = 0x3FFF;
    return fa_action_many(list(
        fa_pair_create(fa_action_send(synth_name, fa_midi_message_create_simple(0xB0 + ch, 0x07, vol / 0x80)), fa_now()),
        fa_pair_create(fa_action_send(synth_name, fa_midi_message_create_simple(0xB0 + ch, 0x27, vol % 0x80)), fa_now())));
}

fa_action_t pan_action(int ch, float pan)
{
    assert(ch >= 0 && ch <= 0x0F && "Channel must be between 0 and 15!");
    int ipan = 0x2000 + round(pan * 0x2000);
    if (ipan < 0) ipan = 0;
    else if (ipan > 0x3FFF) ipan = 0x3FFF;
    return fa_action_many(list(
        fa_pair_create(fa_action_send(synth_name, fa_midi_message_create_simple(0xB0 + ch, 0x0A, ipan / 0x80)), fa_now()),
        fa_pair_create(fa_action_send(synth_name, fa_midi_message_create_simple(0xB0 + ch, 0x2A, ipan % 0x80)), fa_now())));
}

fa_action_t pitch_wheel_action(int ch, float pitch)
{
    assert(ch >= 0 && ch <= 0x0F && "Channel must be between 0 and 15!");
    int ipitch = 0x2000 + round(pitch * 0x2000);
    if (ipitch < 0) ipitch = 0;
    else if (ipitch > 0x3FFF) ipitch = 0x3FFF;
    return fa_action_send(synth_name, fa_midi_message_create_simple(0xE0 + ch, ipitch % 0x80, ipitch / 0x80));
}

fa_action_t sustain_action(int ch, bool down)
{
    assert(ch >= 0 && ch <= 0x0F && "Channel must be between 0 and 15!");
    return fa_action_send(synth_name, fa_midi_message_create_simple(0xB0 + ch, 64, down ? 127 : 0));
}

fa_action_t program_change_action(int ch, int program)
{
    assert(ch >= 0 && ch <= 0x0F && "Channel must be between 0 and 15!");
    if (program < 0) program = 0;
    else if (program > 0x7F) program = 0x7F;
    return fa_action_send(synth_name, fa_midi_message_create_simple(0xC0 + ch, program, 0));
}

fa_action_t bank_select_action(int ch, int bank)
{
    assert(ch >= 0 && ch <= 0x0F && "Channel must be between 0 and 15!");
    if (bank < 0) bank = 0;
    else if (bank > 0x7F) bank = 0x7F;
    return fa_action_send(synth_name, fa_midi_message_create_simple(0xB0 + ch, 0x00, bank));
}

fa_action_t all_notes_off_action()
{
    fa_list_t actions = fa_list_empty();
    for(uint8_t ch = 0; ch < 16; ch++) {
        fa_midi_message_t msg = fa_midi_message_create_simple(0xB0 + ch, 0x7B, 0);
        fa_push_list(fa_action_send(synth_name, msg), actions);
        /*for(uint8_t f0 = 0; f0 < 128; f0++) {
            fa_midi_message_t msg = fa_midi_message_create_simple(msg_note_off + ch, f0, 0);
            fa_push_list(fa_action_send(synth_name, msg), actions);
        }*/
        
    }
    return fa_action_simultaneous(actions);
}


// Helper for absolute_to_relative_times
bool _action_sort(fa_ptr_t a, fa_ptr_t b)
{
    return fa_less_than(fa_pair_second(a), fa_pair_second(b));
}

// Convert a list of pairs (action . time)
// to a list of pairs (action . delta-time-after),
// suited for action_many.
// 
// The passed in list is destroyed.
fa_list_t times_to_delta_times(fa_list_t timeActions)
{
    if (fa_list_is_empty(timeActions)) return timeActions;
    
    timeActions = fa_list_dsort(timeActions, _action_sort);
    timeActions = fa_list_dreverse(timeActions);

    fa_time_t last_time = NULL;
    fa_list_t rel_actions = fa_list_empty();
    while (!fa_list_is_empty(timeActions)) {
        fa_pair_t   head     = fa_list_head(timeActions);
        fa_action_t a        = fa_pair_first(head);
        fa_time_t   abs_time = fa_pair_second(head);
        fa_destroy(head);
        timeActions = fa_list_dtail(timeActions);
        fa_time_t rel_time;
        if (last_time) {
            rel_time = fa_dsubtract(last_time, fa_copy(abs_time));
        } else {
            rel_time = fa_now();
        }
        last_time = abs_time;
        fa_push_list(pair(a, rel_time), rel_actions);
    }
    fa_destroy(timeActions);
    
    // If the last_time is not zero, we have to add a null action to the top of the list
    if (!fa_time_is_zero(last_time)) {
        fa_push_list(pair(fa_action_null(), last_time), rel_actions);
    } else {
        fa_destroy(last_time);
    }
    
    return rel_actions;
}

#define sig_add2(a, b) fa_signal_add(a, b)
#define sig_add3(a, b, c) fa_signal_add(a, fa_signal_add(b, c))
#define sig_add4(a, b, c, d) fa_signal_add(a, fa_signal_add(b, fa_signal_add(c, d)))
#define sig_add(...) VARARG(sig_add, __VA_ARGS__)

#define sig_mul2(a, b) fa_signal_multiply(a, b)
#define sig_mul3(a, b, c) fa_signal_multiply(a, fa_signal_multiply(b, c))
#define sig_mul4(a, b, c, d) fa_signal_multiply(a, fa_signal_multiply(b, fa_signal_multiply(c, d)))
#define sig_mul(...) VARARG(sig_mul, __VA_ARGS__)

fa_signal_t _low_pass (fa_ptr_t curr, fa_signal_t prev) {
    fa_signal_t c = curr;
    return fa_signal_max(sig_add(sig_mul(fa_signal_absolute(prev), fa_signal_constant(0.9995)),
                                 sig_mul(fa_signal_absolute(curr), fa_signal_constant(0.0005))),
                         c);
}

fa_signal_t low_pass(fa_signal_t sig) {
    return fa_signal_loop(_low_pass, sig);
}

fa_list_t construct_signal_tree(bool input, bool output) {
    
    // Input only
    if (!output) {
        return list(sig_mul(fa_signal_input(kMonitorLeft),
                            fa_signal_former(fa_signal_record_external(record_left_name, fa_signal_input(kInputLeft)),
                                             fa_signal_output(0, kLevelLeft,
                                                              low_pass(fa_signal_input(kInputLeft))))),
                                    
                    sig_mul(fa_signal_input(kMonitorRight),
                            fa_signal_former(fa_signal_record_external(record_right_name, fa_signal_input(kInputRight)),
                                             fa_signal_output(0, kLevelRight,
                                                              low_pass(fa_signal_input(kInputRight))))));
    }

    // Synth
    //fa_pair_t synth = fa_signal_synth(synth_name, soundfont_path);
#ifdef _WIN32
    fa_pair_t fa_signal_fluid(fa_string_t name, fluid_synth_t *synth);
    fa_pair_t synth = fluid_synth ? fa_signal_fluid(synth_name, fluid_synth) : NULL;
#else
    #if FA_HAS_FLUIDSYNTH
    fa_pair_t fa_signal_fluid(fa_string_t name, fluid_synth_t *synth);
    fa_pair_t synth = fluid_synth ? fa_signal_fluid(synth_name, fluid_synth) : fa_signal_dls(synth_name);
    #else
    fa_pair_t synth = fa_signal_dls(synth_name);
    #endif
#endif
    
    if (!synth) {
        fa_slog_error("Could not create synth signal!");
        synth = pair(fa_signal_constant(0), fa_signal_constant(0));
    }

    fa_signal_t synth_left = fa_pair_first(synth);
    fa_signal_t synth_right = fa_pair_second(synth);
    fa_destroy(synth); // only destroys the pair
    
    // Audio buffer playback
    fa_pair_t play_buffer = fa_signal_play_buffers(audio_name, audio_buffer_signals);
    fa_signal_t play_buffer_left = fa_pair_first(play_buffer);
    fa_signal_t play_buffer_right = fa_pair_second(play_buffer);
    fa_destroy(play_buffer); // only destroys the pair
    
    // Both input and output
    if (input) {
        return list(sig_add(sig_mul(synth_left, fa_signal_input(kSynthLeft)),
                            sig_mul(play_buffer_left, fa_signal_input(kAudioLeft)),
                            sig_mul(fa_signal_input(kMonitorLeft),
                                    fa_signal_former(fa_signal_record_external(record_left_name, fa_signal_input(kInputLeft)),
                                                     fa_signal_output(0, kLevelLeft,
                                                                      low_pass(fa_signal_input(kInputLeft)))))),
                                    
                    sig_add(sig_mul(synth_right, fa_signal_input(kSynthRight)),
                            sig_mul(play_buffer_right, fa_signal_input(kAudioRight)),
                            sig_mul(fa_signal_input(kMonitorRight),
                                    fa_signal_former(fa_signal_record_external(record_right_name, fa_signal_input(kInputRight)),
                                                     fa_signal_output(0, kLevelRight,
                                                                      low_pass(fa_signal_input(kInputRight)))))));
    }
    // Output only
    else {
        return list(sig_add(sig_mul(synth_left, fa_signal_input(kSynthLeft)),
                            sig_mul(play_buffer_left, fa_signal_input(kAudioLeft))),
                    sig_add(sig_mul(synth_right, fa_signal_input(kSynthRight)),
                            sig_mul(play_buffer_right, fa_signal_input(kAudioRight))));
    }
}

void handle_incoming_midi(fa_time_t time, fa_midi_message_t msg, bool echo_to_playback)
{
    uint8_t statusCh, data1, data2;
    bool simple = fa_midi_message_is_simple(msg);
    if (simple) fa_midi_message_decons(msg, &statusCh, &data1, &data2);
    
    //fa_slog_info("Received MIDI message: ", msg);
    
    fa_ptr_t echo_stream = echo_to_playback ? current_midi_playback_stream : current_midi_echo_stream;
    
    if (echo_stream) {
        // If this is a simple message and midi_echo_channel is set, create a new message with that channel
        if (simple && midi_echo_channel >= 0 && midi_echo_channel < 16) {
            uint8_t status = statusCh & 0xf0;
            fa_midi_message_t new_msg = fa_midi_message_create_simple(status + midi_echo_channel, data1, data2);
            do_schedule_now(fa_action_send(synth_name, new_msg), echo_stream);
            fa_destroy(msg);
        } else {
            // Otherwise, just echo the message
            do_schedule_now(fa_action_send(synth_name, msg), echo_stream);
        }
    } else {
        fa_destroy(msg);
    }
    
    if (simple) {     // don't forward SYSEX via OSC for now
        send_osc_async("/receive/midi", "tiii", timetag_from_time(time), statusCh, data1, data2);
    }

    fa_destroy(time);
}

fa_ptr_t _incoming_midi(fa_ptr_t x, fa_ptr_t time_message)
{
    // fa_print_ln(fa_string_show(timeMessage));
    fa_time_t time         = fa_pair_first(time_message);
    fa_midi_message_t msg  = fa_pair_second(time_message);
    fa_destroy(time_message);
    handle_incoming_midi(time, msg, false);
    return NULL;
}

void _stream_closed(fa_ptr_t data, bool ok)
{
    if (current_midi_playback_stream == current_audio_stream) {
        current_midi_playback_stream = NULL;
    }
    current_audio_stream = NULL;
    if (!ok) {
        fa_slog_error("Audio stream closed unexpectedly");
        send_osc_async("/error", "Ns", "audio-stream-closed");
    }
}

static inline bool audio_device_matches(fa_audio_device_t device, fa_pair_t description) {
    if (!description) return false;
    return (fa_dequal(fa_audio_host_name(device), fa_copy(fa_pair_first(description)))
            && fa_dequal(fa_audio_name(device), fa_copy(fa_pair_second(description))));
}

static inline bool midi_device_matches(fa_midi_device_t device, fa_pair_t description) {
    if (!description) return false;
    return (fa_dequal(fa_midi_host_name(device), fa_copy(fa_pair_first(description)))
            && fa_dequal(fa_midi_name(device), fa_copy(fa_pair_second(description))));
}

static inline bool pair_is_two_empty_strings(fa_pair_t pair) {
    return ((fa_string_length(fa_pair_first(pair)) == 0)
         && (fa_string_length(fa_pair_second(pair)) == 0));
}

void resolve_devices() {
    
    // First, reset current values
    current_audio_input_device = NULL;
    current_audio_output_device = NULL;
    if (current_midi_input_devices) fa_destroy(current_midi_input_devices);
    current_midi_input_devices = fa_list_empty();
    //if (current_midi_output_devices) fa_destroy(current_midi_output_devices);
    current_midi_playback = FA_MIDI_NO_OUTPUT;
    current_midi_echo = FA_MIDI_NO_OUTPUT;
    current_midi_playback_device = NULL;
    current_midi_echo_device = NULL;
    
    fa_list_t midi_devices = fa_midi_all(current_midi_session);
    fa_list_t audio_devices = fa_audio_all(current_audio_session);

    // Audio devices
    // fa_slog_info("In resolve_devices, selected_audio_input_device: ", selected_audio_input_device);
    if (selected_audio_input_device) {
        // a pair of empty strings means default device
        if (pair_is_two_empty_strings(selected_audio_input_device)) {
            current_audio_input_device = fa_audio_default_input(current_audio_session);
        } else {
            fa_for_each(device, audio_devices) {
                if (audio_device_matches(device, selected_audio_input_device) && fa_audio_has_input(device)) {
                    current_audio_input_device = device;
                }
            }
            // Use default input if there was no match
            if (!current_audio_input_device) current_audio_input_device = fa_audio_default_input(current_audio_session);
        }
        if (fa_check(current_audio_input_device)) current_audio_input_device = NULL;
    }
    // fa_slog_info("In resolve_devices, selected_audio_output_device: ", selected_audio_output_device);
    if (selected_audio_output_device) {
        // a pair of empty strings means default device
        if (pair_is_two_empty_strings(selected_audio_output_device)) {
            current_audio_output_device = fa_audio_default_output(current_audio_session);
        } else {
            fa_for_each(device, audio_devices) {
                if (audio_device_matches(device, selected_audio_output_device) && fa_audio_has_output(device)) {
                    current_audio_output_device = device;
                }
            }
            // Use default output if there was no match
            if (!current_audio_output_device) current_audio_output_device = fa_audio_default_output(current_audio_session);
        }
        if (fa_check(current_audio_output_device)) current_audio_output_device = NULL;
    }
    
    // MIDI input devices
    if (!selected_midi_input_devices) { // NULL = all
        fa_for_each(device, midi_devices) {
            if (fa_midi_has_input(device)) fa_push_list(device, current_midi_input_devices);
        }
    } else if (!fa_list_is_empty(selected_midi_input_devices)) {
        fa_for_each(device, midi_devices) {
            if (fa_midi_has_input(device)) {
                fa_for_each(d, selected_midi_input_devices) {
                    if (midi_device_matches(device, d)) {
                        fa_push_list(device, current_midi_input_devices);
                    }
                }
            }
        }
    }
    
    // MIDI playback
    if (selected_midi_playback == FA_MIDI_TO_DEVICE) {
        assert(selected_midi_playback_device && "selected_midi_playback_device is NULL!");
        fa_for_each(device, midi_devices) {
            if (fa_midi_has_output(device) && midi_device_matches(device, selected_midi_playback_device)) {
                current_midi_playback_device = device;
                current_midi_playback = FA_MIDI_TO_DEVICE;
            }
        }
    } else {
        current_midi_playback = selected_midi_playback;
    }
    
    // MIDI echo
    switch (selected_midi_echo) {
    case FA_ECHO_NO_ECHO:
        current_midi_echo = FA_MIDI_NO_OUTPUT;
        break;
    case FA_ECHO_TO_PLAYBACK:
        current_midi_echo_device = current_midi_playback_device;
        current_midi_echo = current_midi_playback;
        break;
    case FA_ECHO_TO_AUDIO:
        current_midi_echo = FA_MIDI_TO_AUDIO;
        break;
    case FA_ECHO_TO_DEVICE:
        assert(selected_midi_echo_device && "selected_midi_echo_device is NULL!");
        fa_for_each(device, midi_devices) {
            if (fa_midi_has_input(device) && midi_device_matches(device, selected_midi_echo_device)) {
                current_midi_echo_device = device;
                current_midi_echo = FA_MIDI_TO_DEVICE;
            }
        }
        break;
    }
    
    fa_slog_info("Audio input:          ", selected_audio_input_device, current_audio_input_device);
    fa_slog_info("Audio output:         ", selected_audio_output_device, current_audio_output_device);
    fa_slog_info("MIDI input:           ", selected_midi_input_devices, current_midi_input_devices);
    fa_slog_info("MIDI playback:        ", fa_from_int8(selected_midi_playback), fa_from_int8(current_midi_playback));
    fa_slog_info("MIDI playback device: ", selected_midi_playback_device, current_midi_playback_device);
    fa_slog_info("MIDI echo:            ", fa_from_int8(selected_midi_echo), fa_from_int8(current_midi_echo));
    fa_slog_info("MIDI echo device:     ", selected_midi_echo_device, current_midi_echo_device);
    
    fa_destroy(midi_devices);  // just the list
    fa_destroy(audio_devices); // just the list
}

void stop_streams() {
    fa_slog_info("Stopping streams...");
    current_clock = fa_clock_standard();
    
    // MIDI input streams
    fa_for_each(stream, current_midi_input_streams) {
        fa_midi_close_stream(stream);
    }
    fa_destroy(current_midi_input_streams);
    current_midi_input_streams = fa_list_empty();
    
    // MIDI echo stream
    if (current_midi_echo_stream && current_midi_echo_stream != current_midi_playback_stream) {
        if (fa_dynamic_get_type(current_midi_echo_stream) == midi_stream_type_repr) {
            fa_midi_close_stream(current_midi_echo_stream);
        }
    }
    current_midi_echo_stream = NULL;
    
    // MIDI playback stream
    if (current_midi_playback_stream) {
        if (fa_dynamic_get_type(current_midi_playback_stream) == midi_stream_type_repr) {
            fa_midi_close_stream(current_midi_playback_stream);
        }
        current_midi_playback_stream = NULL;
    }
    
    // Audio stream
    if (current_audio_stream) {
        fa_audio_close_stream(current_audio_stream);
        current_audio_stream = NULL;
        current_sample_rate = 0;
    }
    
    // Reset counters
    time_echo = 0;
    level_echo = 0;
}

void start_streams() {
#if _WIN32
    fa_string_t dir;
    if (selected_audio_stream_type == BIDIRECTIONAL) dir = fa_string("bidirectional");
    else if (selected_audio_stream_type == INPUT_ONLY) dir = fa_string("input only");
    else if (selected_audio_stream_type == OUTPUT_ONLY) dir = fa_string("output only");
    else dir = fa_string("");
    fa_inform(fa_dappend(fa_string("Starting streams, audio "), dir));
#else
    fa_slog_info("Starting streams");
#endif
    
    if (current_audio_session) {
        // Send settings
        fa_slog_info("Current settings:", session_settings);
        fa_list_t keys = fa_map_get_keys(session_settings);
        fa_for_each (key, keys) {
            fa_ptr_t value = fa_map_get(key, session_settings);
            fa_audio_set_parameter(fa_copy(key), fa_copy(value), current_audio_session);
        }
        fa_destroy(keys);
        
        // Send host settings
        fa_slog_info("Current host settings:");
        fa_slog_info("Input latency:   ", host_input_latency);
        fa_slog_info("Output latency:  ", host_output_latency);
        fa_slog_info("Vector size:     ", host_vector_size);
        if (current_audio_input_device) {
            fa_ptr_t input_latency = fa_map_dget(fa_audio_host_name(current_audio_input_device), host_input_latency);
            if (!input_latency) {
                fa_inform(fa_dappend(fa_string("No input latency setting for host "),
                     fa_audio_host_name(current_audio_input_device)));
                input_latency = fa_map_dget(fa_string(""), host_input_latency);
            }
            if (!input_latency) {
                fa_inform(fa_string("No generic input latency setting, using default"));
                input_latency = fa_from_int16(30);
            }
            fa_ptr_t value = fa_from_double(fa_peek_number(input_latency) / 1000.0);
            fa_ptr_t value_ex = fa_map_dget(fa_dappend(fa_audio_host_name(current_audio_input_device),
                fa_string("#x")), host_input_latency);
            if (value_ex) value_ex = fa_from_double(fa_peek_number(value_ex) / 1000.0);
            else value_ex = fa_copy(value);
            fa_audio_set_parameter(fa_string("input-latency"), value, current_audio_session);
            fa_audio_set_parameter(fa_string("input-latency-exclusive"), value_ex, current_audio_session);
        }
        if (current_audio_output_device) {
            fa_ptr_t output_latency = fa_map_dget(fa_audio_host_name(current_audio_output_device), host_output_latency);
            if (!output_latency) {
                fa_inform(fa_dappend(fa_string("No output latency setting for host "),
                     fa_audio_host_name(current_audio_output_device)));
                output_latency = fa_map_dget(fa_string(""), host_output_latency);
            }
            if (!output_latency) {
                fa_inform(fa_string("No generic output latency setting, using default"));
                output_latency = fa_from_int16(30);
            }
            fa_ptr_t value = fa_from_double(fa_peek_number(output_latency) / 1000.0);
            fa_ptr_t value_ex = fa_map_dget(fa_dappend(fa_audio_host_name(current_audio_output_device),
                fa_string("#x")), host_output_latency);
            if (value_ex) value_ex = fa_from_double(fa_peek_number(value_ex) / 1000.0);
            else value_ex = fa_copy(value);
            fa_audio_set_parameter(fa_string("output-latency"), value, current_audio_session);
            fa_audio_set_parameter(fa_string("output-latency-exclusive"), value_ex, current_audio_session);
        }
        if (current_audio_output_device || current_audio_input_device) {
            fa_ptr_t device = current_audio_input_device ? current_audio_input_device : current_audio_output_device;
            fa_ptr_t vector_size = fa_map_dget(fa_audio_host_name(device), host_vector_size);
            if (!vector_size) {
                fa_inform(fa_dappend(fa_string("No output latency setting for host "),
                     fa_audio_host_name(device)));
                vector_size = fa_map_dget(fa_string(""), host_vector_size);
            }
            if (!vector_size) {
                fa_inform(fa_string("No generic output latency setting, using default"));
                vector_size = fa_from_int16(64);
            }
            fa_ptr_t vector_size_ex = fa_map_dget(fa_dappend(fa_audio_host_name(device), fa_string("#x")), host_vector_size);
            if (!vector_size_ex) vector_size_ex = vector_size;
            fa_audio_set_parameter(fa_string("vector-size"), fa_copy(vector_size), current_audio_session);
            fa_audio_set_parameter(fa_string("vector-size-exclusive"), fa_copy(vector_size_ex), current_audio_session);
        }
    }
    

    assert(selected_audio_stream_type != NO_STREAM);
    if (verbose) {
        fa_slog_info("current_audio_input_device:  ", current_audio_input_device);
        fa_slog_info("current_audio_output_device: ", current_audio_output_device);
    }
    fa_audio_device_t audio_input_device  = selected_audio_stream_type != OUTPUT_ONLY ? current_audio_input_device : NULL;
    fa_audio_device_t audio_output_device = selected_audio_stream_type != INPUT_ONLY ? current_audio_output_device : NULL;
    
#if _WIN32
    if (avoid_wasapi_exclusive_bidirectional && audio_input_device && audio_output_device) {
        // NB: we don't have to test for the device being a WASAPI device, as
        // the exclusive mode flag will only affect WASAPI devices anyway.
        // TODO: we may still want to test it, to avoid the warning for other audio hosts,
        //       to prevent confusion when reading the log file...
        fa_slog_info("Avoiding WASAPI exclusive mode with bidirectional audio stream, falling back to shared mode");
        fa_audio_set_parameter(fa_string("exclusive"), fa_from_bool(false), current_audio_session);
    }
#endif
    
    // Start audio stream first
    if (audio_input_device || audio_output_device) {
        
        if (audio_input_device && audio_output_device) {
            if (!fa_equal(fa_audio_host_name(audio_input_device), fa_audio_host_name(audio_output_device))) {
                fa_log_error(fa_string("Audio host mismatch, setting audio input to NULL"));
                send_osc_async("/error", "Ns", "audio-host-mismatch");
                audio_input_device = NULL;
            }
        }
        
        fa_list_t out_signal = construct_signal_tree(audio_input_device != NULL, audio_output_device != NULL);
        fa_audio_stream_t audio_stream =
            fa_audio_open_stream_with_callbacks(audio_input_device, audio_output_device, just, out_signal,
                                                NULL, _stream_closed, NULL);
        // Check for errors
        if (fa_check(audio_stream)) {
            current_audio_stream = NULL;
            current_sample_rate = 0;
            current_audio_stream_type = NO_STREAM;
            fa_error_t error = (fa_error_t) audio_stream;
            bool handled = false;
            // If a bidirectional stream didn't start, and the sample rates between
            // the input and output device doesn't match, we guess that was the problem.
            if (audio_input_device && audio_output_device) {
                double sr_in  = fa_audio_default_sample_rate(audio_input_device);
                double sr_out = fa_audio_default_sample_rate(audio_output_device);
                fa_ptr_t req_sr = fa_map_dget(fa_string("sample-rate"), session_settings);
                if (sr_in != sr_out && (!req_sr || fa_peek_number(req_sr) == 0)) {
                    fa_log_warning(fa_string("Sample rate mismatch"));
                    send_osc_async("/error", "Nsdd", "sample-rate-mismatch", sr_in, sr_out);
                    handled = true;
                }
            }
            // Otherwise, report a generic error
            if (!handled) {
                char* msg = fa_unstring(fa_error_message(error));
                send_osc_async("/error", "Nss", "could-not-start-stream", msg);
                fa_free(msg);
            }
            fa_destroy(error);
            return;
        }
        current_audio_stream = audio_stream;
        current_sample_rate = fa_audio_stream_sample_rate(current_audio_stream);
        if (audio_input_device && audio_output_device) {
            current_audio_stream_type = BIDIRECTIONAL;
        } else if (audio_input_device) {
            current_audio_stream_type = INPUT_ONLY;
        } else if (audio_output_device) {
            current_audio_stream_type = OUTPUT_ONLY;
        } else {
            assert(false && "Never reached");
        }
        current_clock = fa_audio_get_clock(audio_stream);
        if (current_midi_playback == FA_MIDI_TO_AUDIO) {
            current_midi_playback_stream = audio_stream;
        }
        if (current_midi_echo == FA_MIDI_TO_AUDIO) {
            current_midi_echo_stream = audio_stream;
        }
        
        // Set volumes
        if (current_audio_stream) {
            fa_list_t actions = list(pair(fa_action_set(kSynthLeft, synth_volume), fa_now()),
                                     pair(fa_action_set(kSynthRight, synth_volume), fa_now()),
                                     pair(fa_action_set(kAudioLeft, audio_volume), fa_now()),
                                     pair(fa_action_set(kAudioRight, audio_volume), fa_now()),
                                     pair(fa_action_set(kMonitorLeft, monitor_volume), fa_now()),
                                     pair(fa_action_set(kMonitorRight, monitor_volume), fa_now()));
            do_schedule_now(fa_action_many(actions), current_audio_stream);
        }
        
        // Get some info
        if (current_audio_stream) {
            fa_map_t info = fa_audio_stream_get_info(current_audio_stream);
            fa_slog_info("Actual values reported from PortAudio: ", info);
            fa_free(info);
        }
        
    } else {
        fa_log_warning(fa_string("No audio input or output device, won't start an audio stream"));
    }
    
    // Start one MIDI input stream for each device in current_midi_input_devices
    // Add a listener (message callback) to each stream
    size_t midi_input_count = 0;
    fa_for_each(device, current_midi_input_devices) {
        fa_midi_stream_t midi_stream = fa_midi_open_stream(device);
        error_check(midi_stream, "Could not start MIDI input stream!");
        fa_midi_set_clock(midi_stream, current_clock);
        fa_midi_add_message_callback(_incoming_midi, NULL, midi_stream);
        fa_push_list(midi_stream, current_midi_input_streams);
        midi_input_count++;
    }
    
    // Start MIDI output stream
    size_t midi_output_count = 0;
    if (current_midi_playback_device) {
        fa_midi_stream_t midi_stream = fa_midi_open_stream(current_midi_playback_device);
        error_check(midi_stream, "Could not start MIDI output stream!");
        fa_midi_set_clock(midi_stream, current_clock);
        if (current_midi_echo == FA_MIDI_TO_DEVICE && current_midi_playback_device == current_midi_echo_device) {
             current_midi_echo_stream = midi_stream;
        }
        current_midi_playback_stream = midi_stream;
        midi_output_count++;
    }
    // Start MIDI echo stream
    if (current_midi_echo_device && (current_midi_echo_device != current_midi_playback_device)) {
        current_midi_echo_stream = fa_midi_open_stream(current_midi_playback_device);
        error_check(current_midi_echo_stream, "Could not start MIDI echo stream!");
        fa_midi_set_clock(current_midi_echo_stream, current_clock);
        midi_output_count++;
    }
    
    fa_log_info(fa_string_dappend(fa_format_integral("%d MIDI inputs, ", midi_input_count),
                                  fa_format_integral("%d MIDI outputs", midi_output_count)));
                                  
    // Turn off reverb for all MIDI channels
    if (current_midi_echo_stream) {
        for(uint8_t ch = 0; ch < 16; ch++) {
            fa_midi_message_t msg = fa_midi_message_create_simple(msg_control_change + ch, 0x5B, 0);
            do_schedule_now(fa_action_send(synth_name, msg), current_midi_echo_stream);
        }
    }

    if (current_midi_playback_stream) {
        if (reference_pitch > 0) {
            if (verbose) fa_inform(fa_format("Sending reference pitch %f", reference_pitch));
            do_schedule_now(fa_action_send(synth_name, fa_from_float(reference_pitch)), current_midi_playback_stream);
        } else {
            if (verbose) fa_slog_info("Reference pitch not set");
        }
    }
}

void set_stream_direction(stream_type_t direction) {
    assert(direction != NO_STREAM);
#if _WIN32
    // Always request bidirectional streams if we are not in avoid_wasapi_exlusive_bidirectional mode
    if (!avoid_wasapi_exclusive_bidirectional) direction = BIDIRECTIONAL;
    // Restart streams if needed
    if (recording_state != NOT_RECORDING) {
        fa_slog_info("Won't change stream direction now, since recording_state is ", fa_i8(recording_state));
    } else if (direction != selected_audio_stream_type) {
        if (verbose) fa_slog_info("Stream direction changed, restarting streams");
        stop_streams();
        selected_audio_stream_type = direction;
        start_streams();
        if (verbose) fa_slog_info("Stream direction is now changed");
    }
#endif
}

fa_ptr_t _audio_status_callback(fa_ptr_t session)
{
    send_osc_async("/status-change", "s", "audio");
    return NULL;
}

fa_ptr_t _midi_status_callback(fa_ptr_t session)
{
    send_osc_async("/status-change", "s", "midi");
    return NULL;
}

#ifdef _WIN32
// Get a short (8.3) path on Windows. Does not consume the argument.
char* get_short_cpath(fa_string_t path) {
    wchar_t *wpath = fa_string_to_wstr(path);
    int wlength = GetShortPathNameW(wpath, 0, 0);
    if (!wlength) {
        free(wpath);
        return NULL;
    }
    LPWSTR shortp = (LPWSTR)calloc(wlength, sizeof(WCHAR));
    GetShortPathNameW(wpath, shortp, wlength); // shortp is now the 8.3 path, but as a wide string
    free(wpath);
    int clength = WideCharToMultiByte(CP_OEMCP, 0, shortp, wlength, 0, 0, 0, 0);
    if (!clength) {
        free(shortp);
        return NULL;
    }
    LPSTR cpath = (LPSTR)calloc(clength, sizeof(CHAR));
    WideCharToMultiByte(CP_OEMCP, 0, shortp, wlength, cpath, clength, 0, 0);
    free(shortp);
    return cpath;
}
#endif

void start_sessions() {
    assert(!current_audio_session && "An audio session is already running!");
    assert(!current_midi_session && "A MIDI session is already running!");
    fa_string_t def_host = default_audio_host ? fa_copy(default_audio_host) : NULL;
    current_audio_session = fa_audio_begin_session_with_preferred_host(def_host);
    current_midi_session = fa_midi_begin_session();
    fa_audio_add_status_callback(_audio_status_callback, current_audio_session, current_audio_session);
    fa_midi_add_status_callback(_midi_status_callback, current_midi_session, current_midi_session);

#if FA_HAS_FLUIDSYNTH
    if (soundfont_path) {
        fluid_settings_t *settings = new_fluid_settings();
        fluid_settings_setnum(settings, "synth.gain", 0.6);
        fluid_settings_setint(settings, "synth.threadsafe-api", 0);
        fluid_settings_setint(settings, "synth.verbose", 0);
        fa_inform(fa_string("Creating FluidSynth instance"));
        fluid_synth = new_fluid_synth(settings);
        fa_inform(fa_string_dappend(fa_string("    Loading sound font "), fa_copy(soundfont_path)));

        #ifdef _WIN32        
        // Because fluidsynth uses fopen instead of _wfopen, we convert the filename to 8.3
        char *cpath = get_short_cpath(soundfont_path);
        if (!cpath) {
            fa_fail(fa_string("    Could not load sound font: could not get short path. Does the file exist?"));
            delete_fluid_synth(fluid_synth);
            delete_fluid_settings(settings);
            fluid_synth = NULL;
            return;
        }
        fa_inform(fa_string_dappend(fa_string("    using short path:  "), fa_string_from_utf8(cpath)));
        ////
        #else
        char *cpath = fa_string_to_utf8(soundfont_path);
        #endif
        if (FLUID_FAILED == fluid_synth_sfload(fluid_synth, cpath, true)) {
            fa_fail(fa_string("    Fluidsynth: Could not load sound font"));
            #ifndef _WIN32
            fa_inform(fa_string("    Falling back to DLS synth"));
            #endif
            fluid_settings_t *settings = fluid_synth_get_settings(fluid_synth);
            delete_fluid_synth(fluid_synth);
            delete_fluid_settings(settings);
            fluid_synth = NULL;
        }
        free(cpath);
    }
#endif
}

void stop_sessions() {
    fa_audio_end_session(current_audio_session);
    current_audio_session = NULL;
    fa_midi_end_session(current_midi_session);
    current_midi_session = NULL;
    #if FA_HAS_FLUIDSYNTH
    if (fluid_synth) {
        fa_inform(fa_string("Destroying FluidSynth instance"));
        fluid_settings_t *settings = fluid_synth_get_settings(fluid_synth);
        delete_fluid_synth(fluid_synth);
        delete_fluid_settings(settings);
        fluid_synth = NULL;
    }
    #endif
}

void add_playback_semaphore(oid_t id, fa_string_t signal_name, int slot) {
    fa_ptr_t value = signal_name ? pair(signal_name, fa_i16(slot)) : fa_from_bool(true);
    fa_with_lock(playback_semaphores_mutex) {
        playback_semaphores = fa_map_dset(wrap_oid(id), value, playback_semaphores);
    }
}

bool check_playback_semaphore(fa_ptr_t context, fa_ptr_t dummy)
{
    // bool* bool_ref = context;
    // return *bool_ref;
    //bool* bool_ref = fa_map_get(context, playback_semaphores);
    fa_ptr_t a_bool = fa_map_get(context, playback_semaphores);
    return (a_bool != NULL);
}

bool remove_playback_semaphore(oid_t id) {
    //bool* bool_ref = fa_map_dget(wrap_oid(id), playback_semaphores);
    //if (bool_ref) {
        //fa_free(bool_ref);
        
    bool removed = false;
    fa_with_lock(playback_semaphores_mutex) {
        fa_ptr_t s = fa_map_dget(wrap_oid(id), playback_semaphores);
        if (s) {
            if (fa_dynamic_get_type(s) == pair_type_repr) {
                fa_ptr_t signal_name = fa_pair_first(s);
                fa_ptr_t slot = fa_pair_second(s);
                schedule_now(fa_action_send(signal_name, pair(slot, fa_string("free"))), current_audio_stream);
                fa_destroy(s); // the pair only
            }
            playback_semaphores = fa_map_dremove(wrap_oid(id), playback_semaphores);
            removed = true;
        }
    }
    return removed;
}

static inline void remove_all_playback_semaphores() {
    fa_with_lock(playback_semaphores_mutex) {
        fa_map_t old_semaphores = playback_semaphores;
        playback_semaphores = fa_map_empty();
        fa_destroy(old_semaphores);
    }
}

void add_recording_semaphore(oid_t id) {
    fa_with_lock(recording_semaphores_mutex) {
        recording_semaphores = fa_map_dset(wrap_oid(id), fa_from_bool(true), recording_semaphores);
    }
}

bool check_recording_semaphore(fa_ptr_t context, fa_ptr_t dummy)
{
    fa_ptr_t value = fa_map_get(context, recording_semaphores);
    return (value != NULL); // any value
}

bool remove_recording_semaphore(oid_t id) {
    bool removed = false;
    fa_with_lock(recording_semaphores_mutex) {
        fa_ptr_t s = fa_map_dget(wrap_oid(id), recording_semaphores);
        if (s) {
            //schedule_now(fa_action_send(s, fa_string("free")), current_audio_stream);
            recording_semaphores = fa_map_dremove(wrap_oid(id), recording_semaphores);
            removed = true;
        }
    }
    return removed;
}

static inline void remove_all_recording_semaphores() {
    fa_with_lock(recording_semaphores_mutex) {
        fa_map_t old_semaphores = recording_semaphores;
        recording_semaphores = fa_map_empty();
        fa_destroy(old_semaphores);
    }
}

fa_ptr_t _echo_time(fa_ptr_t context, fa_time_t time, fa_time_t now) {
    //send_osc_async("/time", "h", fa_time_to_milliseconds(time));
    send_osc_async("/time", "t", timetag_from_time(now));
    return NULL;
}

bool _echo_time_p(fa_ptr_t context, fa_ptr_t dummy) {
    return time_echo > 0 && fa_peek_int16(context) == time_echo_id;
}

int start_time_echo()
{
    int new_value;
    fa_with_lock(time_echo_mutex) {
        if (!time_echo) {
            time_echo++;
            time_echo_id++;
            fa_action_t repeat_action = fa_action_repeat(fa_milliseconds(200), 0, fa_action_do_with_time(_echo_time, NULL));
            fa_action_t while_action = fa_action_while(_echo_time_p, fa_from_int16(time_echo_id), repeat_action);
            schedule_relative(sched_delay, while_action, current_midi_playback_stream);
        } else {
            time_echo++;
        }
        new_value = time_echo;
    }
    return new_value;
}

int stop_time_echo()
{
    int new_value;
    fa_with_lock(time_echo_mutex) {
        time_echo--;
        new_value = time_echo;
    }
    return new_value;
}

fa_ptr_t _echo_level(fa_ptr_t context) {
    send_osc_async("/level", "dd", last_level[0], last_level[1]);
    return NULL;
}

double _set_level(fa_ptr_t context, double level) {
    //send_osc_async("/level", "d", level);
    last_level[fa_peek_int16(context)] = level;
    return 0;
}

bool _echo_level_p(fa_ptr_t context, fa_ptr_t dummy) {
    return level_echo > 0;
}

int start_level_echo()
{
    int new_value;
    fa_with_lock(level_echo_mutex) {
        if (!level_echo) {
            level_echo++;
            fa_list_t actions = list(pair(fa_action_get(kLevelLeft,  _set_level, fa_from_int16(0)), fa_now()),
                                     pair(fa_action_get(kLevelRight, _set_level, fa_from_int16(1)), fa_now()),
                                     pair(fa_action_do(_echo_level, NULL), fa_now()));
            fa_action_t repeat_action = fa_action_repeat(fa_milliseconds(50), 0, fa_action_many(actions));
            fa_action_t while_action = fa_action_while(_echo_level_p, NULL, repeat_action);
            schedule_relative(sched_delay, while_action, current_midi_playback_stream);
        } else {
            level_echo++;
        }
        new_value = level_echo;
    }
    return new_value;
}

int stop_level_echo()
{
    int new_value;
    fa_with_lock(level_echo_mutex) {
        if (level_echo > 0) {
            level_echo--;
            new_value = level_echo;
        }
    }
    return new_value;
}

double audio_file_peak(fa_buffer_t buffer)
{
    // Try cache first
    fa_ptr_t peak = fa_buffer_get_meta(buffer, fa_string("peak-level"));
    if (peak) return fa_peek_double(peak);
    
    double max_value = 0;
    // Go through all samples regardless of channel
    for(int i = 0; i < (fa_buffer_size(buffer) / sizeof(double)); i++) {
        double value = fa_buffer_get_double(buffer, i);
        if (value > max_value) max_value = value;
    }
    // Store in cache
    fa_buffer_set_meta(buffer, fa_string("peak-level"), fa_from_double(max_value));
    return max_value;
}

fa_buffer_t audio_curve(fa_ptr_t buffer)
{
    size_t curve_rate = 100;
    size_t size = 0;
    uint8_t *curve = NULL;
    switch (fa_dynamic_get_type(buffer)) {
        case buffer_type_repr:
        {
            fa_ptr_t sr = fa_buffer_get_meta(buffer, fa_string("sample-rate"));
            fa_ptr_t ch = fa_buffer_get_meta(buffer, fa_string("channels"));
            if (!sr || !ch) return NULL;
            size_t sample_rate = fa_peek_number(sr);
            size_t channels = fa_peek_integer(ch);
            double rel_rate = sample_rate / curve_rate;
            size = fa_buffer_size(buffer) / (rel_rate * channels * sizeof(double));
            if (!size) {
                fa_slog_info("audio_curve: curve size is 0, returning NULL");
                return NULL;
            }
            curve = fa_malloc(size);
            if (!curve) {
                assert(false && "Could not allocate memory for audio curve!");
                printf("Could not allocate memory for audio curve!\n");
                return NULL;
            }
            //printf("Curve needs %zu bytes\n", size);
            if (channels == 1) {
                for(int i = 0; i < size; i++) {
                    size_t frame = i * rel_rate;
                    curve[i] = min_uint8(0xFF, 0xFF * fabs(fa_buffer_get_double(buffer, frame * channels + 0)));
                }
            } else {
                for(int i = 0; i < size; i++) {
                    size_t frame = i * rel_rate;
                    double left  = fabs(fa_buffer_get_double(buffer, frame * channels + 0));
                    double right = fabs(fa_buffer_get_double(buffer, frame * channels + 1));
                    curve[i] = min_uint8(0xFF, 0xFF * ((left + right) / 2));
                }
            }
            break;
        }
        case file_buffer_type_repr:
        {
            size_t sample_rate = fa_peek_integer(fa_get_meta(buffer, fa_string("sample-rate")));
            size_t channels = fa_peek_integer(fa_get_meta(buffer, fa_string("channels")));
            size_t sample_size = fa_peek_integer(fa_get_meta(buffer, fa_string("sample-size")));
            fa_sample_type_t sample_type = fa_peek_integer(fa_get_meta(buffer, fa_string("sample-type")));
            size_t frame_size = channels * sample_size;
            double rel_rate = sample_rate / curve_rate;
            size = fa_peek_integer(fa_get_meta(buffer, fa_string("frames"))) / rel_rate;
            curve = fa_malloc(size);
            
            for(int i = 0; i < size; i++) {
                size_t frame = i * rel_rate;
                fa_file_buffer_seek_if_needed(buffer, frame * frame_size);
                double left;
                switch (sample_type) {
                    case float_sample_type:
                    {
                        left = fa_file_buffer_get_float(buffer, frame * channels + 0);
                        break;
                    }
                    case double_sample_type:
                    {
                        left = fa_file_buffer_get_double(buffer, frame * channels + 0);
                        break;
                    }
                }
                curve[i] = min_uint8(0xFF, 0xFF * fabs(left));
            }
            break;
        }
        
        default:
            break;
    }
    if (!curve) return NULL;
    return fa_buffer_dwrap(curve, size);
}

struct upload_buffer_info {
  const char *readptr;
  long sizeleft;
};

// Read function for upload; i.e. the function providing data for the curl upload
// It reads from the provided buffer.
// (The corresponding function for normal buffers is _upload_read_ring_buffer)
static size_t _upload_read_buffer(void *ptr, size_t size, size_t nmemb, void *data)
{
    struct upload_buffer_info *info = (struct upload_buffer_info *)data;
    
    // curl wants size*nmemb bytes

    if(size*nmemb < 1)
        return 0;

    // If there is any data left, read from it
    if (info->sizeleft) {
        size_t bytes = MIN(size*nmemb, info->sizeleft);
        //printf("_upload_read_buffer: uploading %zu bytes (%zu bytes left)\n", bytes, info->sizeleft);
        memcpy(ptr, info->readptr, bytes);
        info->readptr += bytes;
        info->sizeleft -= bytes;
        return size*nmemb;
    }

    // No more data. Return 0 to curl, which tells curl that the transfer is complete.
    return 0;
}

// Read function for upload; i.e. the function providing data for the curl upload
// It reads from the provided ring buffer.
// (The corresponding function for normal buffers is _upload_read_buffer)
static size_t _upload_read_ring_buffer(void *ptr, size_t size, size_t nmemb, void *data)
{
    fa_atomic_ring_buffer_t ring_buffer = (fa_atomic_ring_buffer_t)data;
    
    // // If buffer is closed, return 0
    // if (fa_atomic_ring_buffer_is_closed(ring_buffer)) {
    //     printf("  Closed\n");
    //     return 0;
    // }
	
	// curl wants size*nmemb bytes from us.
    // If there is enough data available, read as much as possible
    if (fa_atomic_ring_buffer_can_read(ring_buffer, size*nmemb)) {
        if (verbose) fa_inform(fa_format_integral("_upload_read_ring_buffer: Reading block of %zu bytes", size*nmemb));
        void *dest = ptr;
        for (size_t i = 0; i < size*nmemb; ++i) {
            bool success = fa_atomic_ring_buffer_read(ring_buffer, dest);
            assert(success && "_upload_read_ring_buffer: Could not read from ring buffer");
            dest++;
        }
        return size*nmemb;
    }

    // Otherwise, read one byte at a time until there is more data available,
	// or until the ring buffer is closed.
    void *dest = ptr;
    if (verbose) printf("_upload_read_ring_buffer: Trying to read %zu bytes one at a time\n", size*nmemb);
    size_t i = 0;
    while (i < size*nmemb) {
        if (fa_atomic_ring_buffer_can_read(ring_buffer, 1)) {
            fa_atomic_ring_buffer_read(ring_buffer, dest);
            dest++;
            i++;
        } else {
            // Cannot read. If it is because the buffer is closed, then return
            if (fa_atomic_ring_buffer_is_closed(ring_buffer)) {
                printf("_upload_read_ring_buffer: Cannot read and buffer was closed. Returning %zu\n", i);
                // If data has already been read during this function call, i will be positive,
                // and curl will call us once more. At that point, no more data is to be read,
                // i will be 0, and returning 0 tells curl that the transfer is complete.
                return i;
            }
            // Otherwise, wait for more data
            // printf("Waiting for data... (ring buffer is not closed)\n");
            fa_thread_sleep(50);
        }
    }
    return size*nmemb;
}

// _upload_write is the function taking care of the server response
static size_t _upload_write(char *ptr, size_t size, size_t nmemb, void *userdata)
{
    fa_buffer_t buffer = (fa_buffer_t)userdata;
    size_t old_size = fa_buffer_size(buffer);
    fa_buffer_dresize(old_size + (size * nmemb), buffer);
    void *dest = fa_buffer_unsafe_address(buffer) + old_size;
    memcpy(dest, ptr, (size * nmemb));
    printf("_upload_write: received %zu bytes (%zu * %zu) from the server\n", size*nmemb, size, nmemb);
    return size*nmemb;
}

struct upload_buffer_args {
    char *osc_path; // reference, do not free!
    oid_t id;
    fa_buffer_t buffer;
    fa_atomic_ring_buffer_t ring_buffer;
    char *url;
    char *cookies;
};

fa_ptr_t upload_buffer(fa_ptr_t context)
{
    if (verbose) fa_slog_info("upload_buffer");
    struct upload_buffer_args *args = context;
    char* osc_path = args->osc_path;
    oid_t id = args->id;
    fa_buffer_t buffer = args->buffer;
    fa_atomic_ring_buffer_t ring_buffer = args->ring_buffer;
    char* url = args->url;
    char* cookies = args->cookies;
    fa_free(args);
    
    if (!buffer && !ring_buffer) {
        fa_slog_warning("Both buffer and ring_buffer is NULL in upload_buffer, aborting upload");
        return NULL;
    }

    //printf("url: %s  cookies: %s\n", url, cookies);
    
    fa_buffer_t result = fa_buffer_create(0);
    
    struct upload_buffer_info info;
    
    fa_slog_info("  In upload thread ", buffer, ring_buffer);
	
    CURL *curl = curl_easy_init();
    if (curl) {
        printf("Uploading to %s ", url);
        curl_easy_setopt(curl, CURLOPT_URL, url); // url is copied by curl
        if (cookies) curl_easy_setopt(curl, CURLOPT_COOKIE, cookies); // as is cookies
        // Set proxy
        if (string_begins_with(url, "http:") && http_proxy) {
            printf("via http proxy %s\n", http_proxy);
            curl_easy_setopt(curl, CURLOPT_PROXY, http_proxy);
            if (http_proxy_userpwd) curl_easy_setopt(curl, CURLOPT_PROXYUSERPWD, http_proxy_userpwd);
        } else if (string_begins_with(url, "https:") && https_proxy) {
            printf("via https proxy %s\n", https_proxy);
            curl_easy_setopt(curl, CURLOPT_PROXY, https_proxy);
            if (https_proxy_userpwd) curl_easy_setopt(curl, CURLOPT_PROXYUSERPWD, https_proxy_userpwd);
        } else {
            printf("(no proxy)\n");
        }
        curl_easy_setopt(curl, CURLOPT_POST, 1L);
        if (ring_buffer) {
            // Ring buffer: enable chunked upload
            struct curl_slist *chunk = NULL;
            chunk = curl_slist_append(chunk, "Transfer-Encoding: chunked");
            curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);
            // Set read function and data
            curl_easy_setopt(curl, CURLOPT_READDATA, ring_buffer);
            curl_easy_setopt(curl, CURLOPT_READFUNCTION, _upload_read_ring_buffer);
            
        } else {
            // Normal buffer: we know the size of the data, so send it in advance
            //curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, fa_buffer_size(buffer));
            
            // *** NOTE ***
            // As of 2015-12-04, the DoReMIR server seems to handle only
            // chunked requests, otherwise it answers with an empty response!
            struct curl_slist *chunk = NULL;
            chunk = curl_slist_append(chunk, "Transfer-Encoding: chunked");
            curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);
            
            // Set read function and data
            info.readptr = fa_buffer_unsafe_address(buffer);
            info.sizeleft = (long)fa_buffer_size(buffer);
            curl_easy_setopt(curl, CURLOPT_READDATA, &info);
            curl_easy_setopt(curl, CURLOPT_READFUNCTION, _upload_read_buffer);
        }

        curl_easy_setopt(curl, CURLOPT_WRITEDATA, result);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _upload_write);
        curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);

        char errbuf[CURL_ERROR_SIZE];
        curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errbuf);
        errbuf[0] = 0;
    
        CURLcode res = curl_easy_perform(curl);

        if (res == CURLE_OK) {
            fa_inform(fa_string("  Upload ok!"));
            long response_code;
            long request_size;
            curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
            curl_easy_getinfo(curl, CURLINFO_REQUEST_SIZE, &request_size);
            //printf("Request was %ld bytes\n", request_size);

            lo_blob blob = lo_blob_from_buffer(result); //lo_blob_new(fa_buffer_size(result), fa_buffer_unsafe_address(result));
            if (blob) {
                send_osc_async(osc_path, "iib", id, response_code, blob);
                lo_blob_free(blob);
            } else {
                send_osc_async(osc_path, "iiN", id, response_code);
            }
        } else {
             // Print and generic error message, and send it back via OSC
             const char *errstr = curl_easy_strerror(res); // points to a string literal that should not be freed
             fa_fail(fa_dappend(fa_format_integral("CURL error %d: ", res), fa_string_from_utf8(errstr)));
             send_osc_async(osc_path, "iFs", id, errstr);
             // Also print specific error message if available
             size_t len = strlen(errbuf);
             if (len) {
                 if (errbuf[len - 1] == '\n') errbuf[len - 1] = 0;      // remove trailing newline
                 fa_fail(fa_dappend(fa_string("    "), fa_string_from_utf8(errbuf)));
             }
        }
        
        // Cleanup
        //curl_slist_free_all(chunk);
        curl_easy_cleanup(curl);
    } else {
        fa_fail(fa_string("Could not allocate curl!"));
        send_osc_async(osc_path, "iFs", id, "Could not allocate curl");
    }
    
    fa_slog_info("  Cleaning up upload, destroying buffers");
    fa_slog_info("     buffer: ", buffer);
    fa_slog_info("     ring_buffer: ", ring_buffer);
    free(url);
    if (cookies) free(cookies);
    if (buffer) fa_destroy(buffer);
    if (ring_buffer) fa_atomic_ring_buffer_release_reference(ring_buffer);
    fa_destroy(result);
    
    fa_slog_info("  End of upload function");
    
    return NULL;
}

fa_thread_t upload_buffer_async(oid_t id, fa_buffer_t buffer, char* url, char* cookies)
{
    struct upload_buffer_args *args = fa_new_struct(upload_buffer_args);
    args->osc_path = "/audio-file/upload";
    args->id = id;
    args->buffer = buffer;
    args->ring_buffer = NULL;
    args->url = strdup(url);
    args->cookies = strdup_or_null(cookies);
    
    fa_thread_t thread = fa_thread_create(upload_buffer, args, fa_string("Upload buffer"));
    return thread;
}

fa_thread_t upload_ring_buffer_async(oid_t id, fa_atomic_ring_buffer_t ring_buffer, char* url, char* cookies)
{
    struct upload_buffer_args *args = fa_new_struct(upload_buffer_args);
    args->osc_path = "/recording/result";
    args->id = id;
    args->buffer = NULL;
    args->ring_buffer = ring_buffer;
    args->url = strdup(url);
    args->cookies = strdup_or_null(cookies);
    
    fa_atomic_ring_buffer_take_reference(ring_buffer);
    
    fa_thread_t thread = fa_thread_create(upload_buffer, args, fa_string("Upload ring buffer"));
    return thread;
}


struct recording_thread_args {
    oid_t id;
    fa_atomic_ring_buffer_t ring_buffer;
    char *filename;
    char *url;
    char *cookies;
    long sample_rate;
    long channels;
};

// _recording_receive: this is an IO callback, called with streamed ogg data from the on-going
// recording. The data is written to a ring_buffer, which is uploaded in parallel in another
// thread. Note that this callback is only used for upload; for file-only recording, it is
// never inserted into the IO chain.
void _recording_receive(fa_ptr_t context, fa_buffer_t buffer) {
    fa_atomic_ring_buffer_t ring_buffer = context;
    if (!buffer) {
        if (verbose) fa_slog_info("_recording_receive: Received NULL, closing ring buffer");
        fa_atomic_ring_buffer_close(ring_buffer);
    // } else if (recording_state == RECORDING_STOPPING && !fa_atomic_ring_buffer_is_closed(ring_buffer)) {
    //     printf("_recording_receive: recording_state == RECORDING_STOPPING, closing ring buffer\n");
    //     fa_atomic_ring_buffer_close(ring_buffer);
    } else {
        uint8_t *ptr = fa_buffer_unsafe_address(buffer);
        size_t size = fa_buffer_size(buffer);
        if (verbose) fa_inform(fa_format_integral("_recording_receive: Received %zu bytes of ogg", size));
        if (fa_atomic_ring_buffer_can_write(ring_buffer, size)) {
            for (size_t i = 0; i < fa_buffer_size(buffer); i++) {
                 fa_atomic_ring_buffer_write(ring_buffer, *ptr);
                 ptr++;
             }
        } else {
            // TODO: handle this error!
            fa_fail(fa_string("_recording_receive: Upload ring buffer overflow!"));
        }
    }
}

fa_ptr_t _recording_thread(fa_ptr_t context)
{
    if (verbose) fa_slog_info("_recording_thread start (before init)");
    struct recording_thread_args *args = context;
    oid_t id = args->id;
    fa_atomic_ring_buffer_t ring_buffer = args->ring_buffer;
    char* filename = args->filename;
    char* url = args->url;
    char* cookies = args->cookies;
    long sr = args->sample_rate;
    long ch = args->channels;
    fa_free(args);
    
    fa_slog_info("_recording_thread start");

    //printf("url: %s  cookies: %s\n", url, cookies);
    
    fa_io_source_t source = fa_io_from_ring_buffer(ring_buffer);
    fa_io_sink_t sink = NULL;
    fa_atomic_ring_buffer_t upload_ring_buffer = NULL;
    
    if (url && filename) {
        // If url is specified, create the file sink as a split filter
        fa_io_sink_t file_sink = fa_io_write_file(fa_string_from_utf8(filename));
        source = fa_io_apply(fa_io_apply(source, fa_io_split(file_sink)), fa_io_create_ogg_encoder(sr, ch, default_ogg_quality));
    } else if (url) {
        source = fa_io_apply(source, fa_io_create_ogg_encoder(sr, ch, default_ogg_quality));
    } else {
        if (verbose) fa_slog_info("Creating file sink");
        sink = fa_io_write_file(fa_string_from_utf8(filename));
    }
    
    // if (verbose) fa_slog_info("_recording_thread 3");
    
    // If sink is unspecified, it should be the upload sink
    // Start a new thread for the upload and send data to it though a second ring buffer
    if (!sink) {
        upload_ring_buffer = fa_atomic_ring_buffer(1048576);
        fa_atomic_ring_buffer_take_reference(upload_ring_buffer);
        fa_destroy(upload_ring_buffer); // delayed destruction because of the reference
        sink = (fa_io_sink_t) fa_io_create_simple_filter(_recording_receive, NULL, upload_ring_buffer);
        fa_thread_t thread = upload_ring_buffer_async(id, upload_ring_buffer, url, cookies);
        fa_thread_detach(thread);
    }
    
    if (verbose) fa_inform(fa_format_integral("_recording_thread 4, recording_state: %d", recording_state));
    
    if (check_recording_semaphore(wrap_oid(id), NULL)) {
        if (recording_state == RECORDING_RUNNING || recording_state == RECORDING_INITIALIZING) {
            // If still initializing, wait here...
            while (recording_state == RECORDING_INITIALIZING) {
                fa_thread_sleep(100);
            }

            if (recording_state == RECORDING_RUNNING) {
                // This will block until the source is drained
                fa_slog_info("Recording thread: recording_state is now RUNNING, calling io_run");
                fa_io_run(source, sink);
                
                
            } else {
                fa_slog_info("Recording thread: recording_state went from INITIALIZING to STOPPING, skipping io_run");
            }
        } else {
            fa_slog_info("Recording thread: recording_state is not RUNNING or INITIALIZING, skipping io_run");
        }
    } else {
        fa_slog_info("Recording thread: sempahore is not set, skipping io_run");
    }
    
	if (verbose) fa_slog_info("_recording_thread 4.5, before detaching sink");
	
    // Make sure sink is detached (especially important if io_run is never called,
    // otherwise the upload callback will never return!)
    //
    // NB: because of a bug(?) in fa_io_run, a NULL value was never pushed onto
    // the sink after the finished run. That was probably the underlying cause
    // of the problems that made us ALWAYS run this push. Now that bug is fixed,
    // however there is no problem calling _recording_receive twice with NULL,
    // so we leave it here. / ER 2017-10-11
    fa_io_push(sink, NULL);
    
    if (verbose) fa_slog_info("_recording_thread 5");
        
    // Cleanup
    if (filename) free(filename);
    if (url) free(url);
    if (cookies) free(cookies);
    fa_destroy(source);
    fa_destroy(sink);
    if (upload_ring_buffer) fa_atomic_ring_buffer_release_reference(upload_ring_buffer);
    //fa_destroy(ring_buffer); // currently, we are using a global ring buffer, so don't free it
    //fa_destroy(result);
    
    fa_with_lock(recording_state_mutex) {
        if (recording_state != RECORDING_RUNNING && recording_state != RECORDING_STOPPING) {
            fa_warn(fa_string_format_integral("  !! recording_state is %d", recording_state));
        }
        recording_state = NOT_RECORDING;
    }
    
    fa_slog_info("Sent /recording/stopped");
    send_osc_async("/recording/stopped", "is", id, "stopped");
    
    fa_slog_info("End of recording thread function");
    
    return NULL;
}


fa_thread_t create_recording_thread(oid_t id, fa_atomic_ring_buffer_t ring_buffer, char* filename, char* url, char* cookies,
                                    long sample_rate, long channels)
{
    struct recording_thread_args *args = fa_new_struct(recording_thread_args);
    args->id          = id;
    args->ring_buffer = ring_buffer;
    args->filename    = strdup_or_null(filename);
    args->url         = strdup_or_null(url);
    args->cookies     = strdup_or_null(cookies);
    args->sample_rate = sample_rate;
    args->channels    = channels;
    
    fa_thread_t thread = fa_thread_create(_recording_thread, args, fa_string("Recording thread"));
    return thread;
}

void buffer_hint(fa_ptr_t buffer, size_t frames)
{
    if (fa_dynamic_get_type(buffer) == file_buffer_type_repr) {
        fa_file_buffer_t fb = buffer;
        size_t channels = fa_peek_integer(fa_get_meta(buffer, fa_string("channels")));
        size_t sample_size = fa_peek_integer(fa_get_meta(buffer, fa_string("sample-size")));
        fa_file_buffer_hint(fb, frames * channels * sample_size);
    }
}

#endif

