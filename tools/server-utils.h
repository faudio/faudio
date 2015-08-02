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

void schedule(fa_time_t time, fa_action_t action, fa_ptr_t stream) {
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

void schedule_relative(fa_time_t time, fa_action_t action, fa_ptr_t stream) {
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
fa_list_t times_to_delta_times(fa_list_t timeActions) //, fa_action_t before, fa_action_t after)
{
    //fa_log_list_count();
    timeActions = fa_list_dsort(timeActions, _action_sort);
    // if (before) {
    //     fa_push_list(pair(before, fa_now()), timeActions);
    // }
    //fa_log_list_count();
    timeActions = fa_list_dreverse(timeActions);
    // if (after) {
    //     fa_push_list(pair(after, fa_copy(fa_pair_second(fa_list_head(timeActions)))), timeActions);
    // }
    
    
    //fa_log_list_count();
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


fa_list_t construct_output_signal_tree() {
    
    // Synth
    #ifdef _WIN32
    fa_pair_t synth = fa_signal_synth(synth_name, fa_string("C:\\sf.sf2"));
    #else
    fa_pair_t synth = fa_signal_dls(synth_name);
    #endif
    
    fa_slog_info("synth: ", synth);
    
    fa_signal_t synth_left = fa_pair_first(synth);
    fa_signal_t synth_right = fa_pair_second(synth);
    fa_destroy(synth); // only destroys the pair
    
    // Audio buffer playback
    fa_pair_t play_buffer = fa_signal_play_buffer(audio_name);
    printf("pointer is now %p\n", play_buffer);

    fa_slog_info("play_buffer: ", play_buffer);
    
    fa_signal_t play_buffer_left = fa_pair_first(play_buffer);
    fa_signal_t play_buffer_right = fa_pair_second(play_buffer);
    fa_destroy(play_buffer); // only destroys the pair
    
    fa_list_t tree = list(fa_signal_add(synth_left, play_buffer_left),
                          fa_signal_add(synth_right, play_buffer_right));
    
    return tree;
}

void stop_streams() {
    fa_slog_info("Stopping streams...");
    if (current_audio_stream) {
        fa_audio_close_stream(current_audio_stream);
        current_audio_stream = NULL;
        current_midi_echo_stream = NULL;
        current_clock = fa_clock_standard();
    }
}

void start_streams() {
    fa_slog_info("Starting streams...");
    if (current_audio_output_device) {
        fa_list_t out_signal = construct_output_signal_tree();
        fa_audio_stream_t stream = fa_audio_open_stream(NULL, current_audio_output_device, just, out_signal);
        if (fa_check(stream)) {
            fa_log_error(fa_string("Could not start audio stream!"));
            fa_error_log(stream, NULL);
            return;
        }
        current_audio_stream = stream;
        current_clock = fa_audio_get_clock(stream);
        if (selected_midi_echo == FA_ECHO_AUDIO) {
            current_midi_echo_stream = stream;
        }
    } else {
        fa_log_warning(fa_string("No audio output device, won't start an audio stream"));
    }
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
    case LO_BLOB:       assert(false && "creating buffers from blobs not yet supported");
    case LO_INT64:      return fa_from_int64(val64.i);
    case LO_DOUBLE:     return fa_from_double(val64.f);
    case LO_SYMBOL:     return fa_string((char *) data);
    case LO_TRUE:       return fa_from_bool(true);
    case LO_FALSE:      return fa_from_bool(false);
    case LO_NIL:        return fa_from_bool(false);
    default:
        fprintf(stderr, "warning in create_fa_value: unhandled type: %c\n", type);
        return NULL;
    }
}

void add_playback_semaphore(oid_t id, fa_string_t signal_name) {
    if (!signal_name) signal_name = fa_from_bool(true);
    fa_with_lock(playback_semaphore_mutex) {
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
    fa_with_lock(playback_semaphore_mutex) {
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
    fa_with_lock(playback_semaphore_mutex) {
        fa_map_t old_semaphores = playback_semaphores;
        playback_semaphores = fa_map_empty();
        fa_destroy(old_semaphores);
    }
}

fa_ptr_t _echo_time(fa_ptr_t context, fa_time_t time) {
    send_osc_async("/time", "h", fa_time_to_milliseconds(time));
    return NULL;
}

bool _echo_time_p(fa_ptr_t context, fa_ptr_t dummy) {
    return time_echo > 0;
}

int start_time_echo()
{
    int new_value;
    fa_with_lock(playback_semaphore_mutex) {
        if (!time_echo) {
            time_echo++;
            fa_action_t repeat_action = fa_action_repeat(fa_milliseconds(200), fa_action_do_with_time(_echo_time, NULL));
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
    fa_with_lock(playback_semaphore_mutex) {
        time_echo--;
        new_value = time_echo;
    }
    return new_value;
}

