#include "fa/fa.h"
#include "server-types.h"
#include "server-globals.h"

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

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

uint8_t min_uint8(uint8_t a, uint8_t b) { return a < b ? a : b; }

#define send_osc(m, s, ...) lo_send_from(lo_message_get_source(m), (lo_server)s, LO_TT_IMMEDIATE, __VA_ARGS__)
#define send_osc_async(...) if (last_address) lo_send_from(last_address, server, LO_TT_IMMEDIATE, __VA_ARGS__)

#define check_id(id) \
if (id > last_used_id) {    \
    last_used_id = id;      \
} else {                    \
    fa_fail(fa_string_dappend(fa_string_format_integral("ID %zu is lower than", id), \
                              fa_string_format_integral(" last used ID (%zu)", last_used_id))); \
    return 0; \
}

#define error_check(_obj, _msg)     \
if (fa_check(_obj)) {               \
    fa_log_error(fa_string(_msg));  \
    fa_error_log(_obj, NULL);       \
    return;                         \
}


#define safe_peek_i32(ptr) (ptr ? (int32_t) fa_peek_number(ptr) : 0)

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
    }
}

void schedule(fa_time_t time, fa_action_t action, fa_ptr_t stream) {
    if (in_bundle) {
        if (bundle_stream) {
            assert(bundle_stream == stream && "Cannot send to different streams in same bundle");
        } else {
            bundle_stream = stream;
        }
        fa_push_list(pair(action, time), bundle_actions);
    } else {
        do_schedule(time, action, stream);
    }
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
    case LO_STRING:     return fa_string((char *) data);
    //case LO_BLOB:     return fa_buffer_wrap(data + 4, val32.i, NULL, NULL); // TODO: copy memory
    case LO_BLOB:       assert(false && "creating buffers from blobs not yet supported");
    case LO_INT64:      return fa_from_int64(val64.i);
    case LO_DOUBLE:     return fa_from_double(val64.f);
    case LO_SYMBOL:     return fa_string((char *) data);
    case LO_CHAR:       return fa_string_single(val32.c); // Encoding beyond ascii?
    //case LO_MIDI: 
    case LO_TRUE:       return fa_from_bool(true);
    case LO_FALSE:      return fa_from_bool(false);
    case LO_NIL:        return NULL; // ?
    default:
        fa_fail(fa_string("liblo_to_faudio: Cannot handle that type"));
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
        for(uint8_t f0 = 0; f0 < 128; f0++) {
            fa_midi_message_t msg = fa_midi_message_create_simple(msg_note_off + ch, f0, 0);
            fa_push_list(pair(fa_action_send(synth_name, msg), fa_now()), actions);
        }
    }
    return fa_action_many(actions);
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

fa_list_t construct_output_signal_tree() {
    
    // Synth
    #ifdef _WIN32
    fa_pair_t synth = fa_signal_synth(synth_name, fa_string("C:\\sf.sf2")); // TODO
    #else
    fa_pair_t synth = fa_signal_dls(synth_name);
    #endif
    
    fa_signal_t synth_left = fa_pair_first(synth);
    fa_signal_t synth_right = fa_pair_second(synth);
    fa_destroy(synth); // only destroys the pair
    
    // Audio buffer playback
    fa_pair_t play_buffer = fa_signal_play_buffer(audio_name);
    fa_signal_t play_buffer_left = fa_pair_first(play_buffer);
    fa_signal_t play_buffer_right = fa_pair_second(play_buffer);
    fa_destroy(play_buffer); // only destroys the pair
        
    fa_list_t tree =
        list(sig_add(sig_mul(synth_left, fa_signal_input(kSynthLeft)),
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
    
    return tree;
}

void handle_incoming_midi(fa_time_t time, fa_midi_message_t msg)
{
    uint8_t status, data1, data2;
    bool simple = fa_midi_message_is_simple(msg);
    if (simple) fa_midi_message_decons(msg, &status, &data1, &data2);
    
    //fa_slog_info("Received MIDI message: ", msg);
    
    if (current_midi_echo_stream) {
        do_schedule_now(fa_action_send(synth_name, msg), current_midi_echo_stream);
    } else {
        fa_destroy(msg);
    }
    
    if (simple) {     // don't forward SYSEX via OSC for now
        send_osc_async("/receive/midi", "tiii", timetag_from_time(time), status, data1, data2);
    }

    fa_destroy(time);
}

fa_ptr_t _incoming_midi(fa_ptr_t x, fa_ptr_t time_message)
{
    // fa_print_ln(fa_string_show(timeMessage));
    fa_time_t time         = fa_pair_first(time_message);
    fa_midi_message_t msg  = fa_pair_second(time_message);
    fa_destroy(time_message);
    handle_incoming_midi(time, msg);
    return NULL;
}

void stop_streams() {
    fa_slog_info("Stopping streams...");
    current_midi_echo_stream = NULL;
    // MIDI input streams
    fa_for_each(stream, current_midi_input_streams) {
        fa_midi_close_stream(stream);
    }
    fa_destroy(current_midi_input_streams);
    current_midi_input_streams = fa_list_empty();
    // MIDI output streams
    fa_for_each(stream, current_midi_output_streams) {
        fa_midi_close_stream(stream);
    }
    fa_destroy(current_midi_output_streams);
    current_midi_output_streams = fa_list_empty();
    // Audio stream
    if (current_audio_stream) {
        fa_audio_close_stream(current_audio_stream);
        current_audio_stream = NULL;
    }
    current_clock = fa_clock_standard();
}

void start_streams() {
    fa_slog_info("Starting streams...");
    
    // Start audio stream first
    if (current_audio_output_device) {
        fa_list_t out_signal = construct_output_signal_tree();
        fa_audio_stream_t audio_stream =
            fa_audio_open_stream(current_audio_input_device, current_audio_output_device, just, out_signal);
        error_check(audio_stream, "Could not start audio stream!");
        current_audio_stream = audio_stream;
        current_clock = fa_audio_get_clock(audio_stream);
        if (selected_midi_echo == FA_ECHO_AUDIO) {
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
    } else {
        fa_log_warning(fa_string("No audio output device, won't start an audio stream"));
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
    
    // Start one MIDI output stream for each device in current_midi_output_devices
    size_t midi_output_count = 0;
    fa_for_each(device, current_midi_output_devices) {
        fa_midi_stream_t midi_stream = fa_midi_open_stream(device);
        error_check(midi_stream, "Could not start MIDI output stream!");
        fa_midi_set_clock(midi_stream, current_clock);
        if (selected_midi_echo == FA_ECHO_DEVICE && device == selected_midi_echo_device) {
            current_midi_echo_stream = midi_stream;
        }
        fa_push_list(midi_stream, current_midi_output_streams);
        midi_output_count++;
    }
    fa_log_info(fa_string_dappend(fa_format_integral("%d MIDI inputs, ", midi_input_count),
                                  fa_format_integral("%d MIDI outputs", midi_output_count)));
                                  
    // Turn off reverb for all channels
    if (current_midi_echo_stream) {
        for(uint8_t ch = 0; ch < 16; ch++) {
            fa_midi_message_t msg = fa_midi_message_create_simple(msg_control_change + ch, 0x5B, 0);
            do_schedule_now(fa_action_send(synth_name, msg), current_midi_echo_stream);
        }
    }
}

void add_playback_semaphore(oid_t id, fa_string_t signal_name) {
    if (!signal_name) signal_name = fa_from_bool(true);
    fa_with_lock(playback_semaphores_mutex) {
        playback_semaphores = fa_map_dset(wrap_oid(id), signal_name, playback_semaphores);
    }
    //return a_bool;
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
            if (fa_dynamic_get_type(s) == string_type_repr) {
                schedule_relative(fa_now(), fa_action_send(s, fa_string("free")), current_audio_stream);
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

fa_ptr_t _echo_time(fa_ptr_t context, fa_time_t time, fa_time_t now) {
    //send_osc_async("/time", "h", fa_time_to_milliseconds(time));
    send_osc_async("/time", "t", timetag_from_time(now));
    return NULL;
}

bool _echo_time_p(fa_ptr_t context, fa_ptr_t dummy) {
    return time_echo > 0;
}

int start_time_echo()
{
    int new_value;
    fa_with_lock(time_echo_mutex) {
        if (!time_echo) {
            time_echo++;
            fa_action_t repeat_action = fa_action_repeat(fa_milliseconds(200), 0, fa_action_do_with_time(_echo_time, NULL));
            fa_action_t while_action = fa_action_while(_echo_time_p, NULL, repeat_action);
            schedule_relative(fa_now(), while_action, current_midi_echo_stream);
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
            schedule_relative(fa_now(), while_action, current_midi_echo_stream);
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

lo_blob audio_curve(fa_buffer_t buffer)
{
    fa_ptr_t sr = fa_buffer_get_meta(buffer, fa_string("sample-rate"));
    fa_ptr_t ch = fa_buffer_get_meta(buffer, fa_string("channels"));
    if (!sr || !ch) return NULL;
    size_t sample_rate = fa_peek_number(sr);
    size_t channels = fa_peek_integer(ch);
    size_t curve_rate = 200;
    double rel_rate = sample_rate / curve_rate;
    size_t size = fa_buffer_size(buffer) / (rel_rate * channels * sizeof(double));
    uint8_t *curve = fa_malloc(size);
    //printf("Curve needs %zu bytes\n", size);
    if (channels == 1) {
        //double last_value = 0;
        
        for(int i = 0; i < size; i++) {
            size_t frame = i * rel_rate;
            //double value = fabs(fa_buffer_get_double(buffer, frame * channels + 0));
            //value = (value * 0.01) + (last_value * 0.99);
            //curve[i] = min_uint8(0xFF, 0xFF * value);
            //last_value = value;
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
    lo_blob blob = lo_blob_new(size, curve);
    printf("Sending curve of %zu bytes\n", size);
    fa_free(curve);
    return blob;
}

