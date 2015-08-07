/*
*  Copyright (C) 2015 Erik Ronström
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "lo/lo.h"

#include <fa/fa.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/string.h>
#include <fa/func_ref.h>
#include "common.h"

#include "server-types.h"
#include "server-globals.h"
#include "server-utils.h"

#define define_handler(name) \
  int name ## _handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)

int done = 0;


void liblo_error(int num, const char *m, const char *path);

define_handler(generic);

define_handler(quit);
define_handler(restart);
define_handler(settings);

define_handler(test);
define_handler(stats);
define_handler(simple_midi);
define_handler(simple_note);
define_handler(play_midi);
define_handler(play_audio);
define_handler(stop);

define_handler(time);
define_handler(all_devices);
define_handler(current_devices);

define_handler(load_audio_file);
define_handler(close_audio_file);
define_handler(save_audio_file);
define_handler(audio_file_curve);
define_handler(audio_file_peak);

define_handler(main_volume);
define_handler(pan);
define_handler(program_change);
define_handler(pitch_wheel);
define_handler(sustain);
define_handler(channel_reset);

define_handler(volume);

define_handler(start_recording);
define_handler(stop_recording);


int main()
{
    fa_set_log_std();
  
    const char *port = "7770";
  
    /* start a new server on port 7770 */
    lo_server_thread st = lo_server_thread_new_with_proto(port, LO_TCP, liblo_error);
    server = lo_server_thread_get_server(st);

    /* add method that will match any path and args */
    lo_server_thread_add_method(st, NULL, NULL, generic_handler, server);
        
    lo_server_thread_add_method(st, "/stats", "", stats_handler, server);

    /* Quitting, restart */
    lo_server_thread_add_method(st, "/quit", "", quit_handler, server);
    lo_server_thread_add_method(st, "/restart", "", restart_handler, server);
    lo_server_thread_add_method(st, "/restart", "i", restart_handler, server);
    
    /* Settings */
    lo_server_thread_add_method(st, "/set/latency",            "f", settings_handler, "latency");
    lo_server_thread_add_method(st, "/set/input-latency",      "f", settings_handler, "input-latency");
    lo_server_thread_add_method(st, "/set/output-latency",     "f", settings_handler, "output-latency");
    lo_server_thread_add_method(st, "/set/sample-rate",        "f", settings_handler, "sample-rate");
    lo_server_thread_add_method(st, "/set/vector-size",        "i", settings_handler, "vector-size");
    lo_server_thread_add_method(st, "/set/scheduler-interval", "i", settings_handler, "scheduler-interval");
    lo_server_thread_add_method(st, "/set/exclusive",          "T", settings_handler, "exclusive");
    lo_server_thread_add_method(st, "/set/exclusive",          "F", settings_handler, "exclusive");
    lo_server_thread_add_method(st, "/set/exclusive",          "N", settings_handler, "exclusive");
      
    /* Get info */
    lo_server_thread_add_method(st, "/time", "", time_handler, server);
    lo_server_thread_add_method(st, "/all/devices", "", all_devices_handler, server);
    lo_server_thread_add_method(st, "/current/devices", "", current_devices_handler, server);

    /* Send raw midi messages */
    lo_server_thread_add_method(st, "/send/midi", "ii",  simple_midi_handler, server);
    lo_server_thread_add_method(st, "/send/midi", "iii", simple_midi_handler, server);
    /* Send note immediately */
    lo_server_thread_add_method(st, "/send/note", "iii", simple_note_handler, server); // f0, velocity, length (ms)
    lo_server_thread_add_method(st, "/send/note", "iiii", simple_note_handler, server); // f0, velocity, length (ms), channel
    
    /* Playback */
    //  /play/audio  id,  audio id,  skip (ms),  start-time (ms),  repeat-interval (ms)
    //  /play/midi   id,  data,  start-time (ms),  repeat-interval (ms)
    //  /stop        id
    lo_server_thread_add_method(st, "/play/audio", "ii", play_audio_handler, server);
    lo_server_thread_add_method(st, "/play/audio", "iif", play_audio_handler, server);
    lo_server_thread_add_method(st, "/play/audio", "iiff", play_audio_handler, server);
    lo_server_thread_add_method(st, "/play/audio", "iifff", play_audio_handler, server);
    lo_server_thread_add_method(st, "/play/midi", "ib", play_midi_handler, server);
    lo_server_thread_add_method(st, "/play/midi", "ibf", play_midi_handler, server);
    lo_server_thread_add_method(st, "/play/midi", "ibff", play_midi_handler, server);
    lo_server_thread_add_method(st, "/stop", "i", stop_handler, server);

    /* Audio files handling */
    lo_server_thread_add_method(st, "/audio-file/load",  "is", load_audio_file_handler, server);  // audio id, path
    lo_server_thread_add_method(st, "/audio-file/close", "i",  close_audio_file_handler, server); // audio id
    lo_server_thread_add_method(st, "/audio-file/save",  "i",  save_audio_file_handler, server);  // audio id
    lo_server_thread_add_method(st, "/audio-file/save",  "is", save_audio_file_handler, server);  // audio id, path
    lo_server_thread_add_method(st, "/audio-file/curve", "i",  audio_file_curve_handler, server); // audio id
    lo_server_thread_add_method(st, "/audio-file/peak",  "i",  audio_file_peak_handler, server);  // audio id
    
    /* Send midi control messages */
    lo_server_thread_add_method(st, "/send/main-volume",    "ii", main_volume_handler, server);
    lo_server_thread_add_method(st, "/send/pan",            "if", pan_handler, server);
    lo_server_thread_add_method(st, "/send/program-change", "iii", program_change_handler, server);
    lo_server_thread_add_method(st, "/send/pitch-wheel",    "if", pitch_wheel_handler, server);
    lo_server_thread_add_method(st, "/send/sustain",        "ii", sustain_handler, server); // ch, sustain (0=up or 1=down)
    lo_server_thread_add_method(st, "/send/channel-reset",  "i", channel_reset_handler, server);
    lo_server_thread_add_method(st, "/send/channel-reset",  "iifiifi", channel_reset_handler, server);
    
    /* Volume control */
    lo_server_thread_add_method(st, "/volume/synth",   "f", volume_handler, (void*)((uint32_t)kSynthLeft));
    lo_server_thread_add_method(st, "/volume/audio",   "f", volume_handler, (void*)((uint32_t)kAudioLeft));
    lo_server_thread_add_method(st, "/volume/monitor", "f", volume_handler, (void*)((uint32_t)kMonitorLeft));
    
    lo_server_thread_add_method(st, "/recording/start", "iff", start_recording_handler, server); //
    lo_server_thread_add_method(st, "/recording/stop",  "i", stop_recording_handler, server);

    fa_with_faudio() {
        
        fa_clock_initialize();
        
        init_globals();

        current_audio_session = fa_audio_begin_session();
        current_audio_output_device = fa_audio_default_output(current_audio_session);
        current_midi_session = fa_midi_begin_session();
        
        start_streams(); // server-utils.h

        lo_server_thread_start(st);
    
        printf("Listening on TCP port %s\n", port);

        while (!done) {
#ifdef WIN32
            Sleep(1);
#else
            usleep(1000);
#endif
        }
    
        stop_streams();
        
        fa_audio_end_session(current_audio_session);
        fa_midi_end_session(current_midi_session);
        
        destroy_globals();
      
    }

    lo_server_thread_free(st);
    
    fa_log_region_count(fa_string("At shutdown"));
    fa_list_log_count();
    fa_time_log_count();
    fa_pair_log_count();
    fa_pair_left_log_count();
    fa_string_log_count();
    fa_func_ref_log_count();
    fa_action_log_count();

    return 0;
}

void liblo_error(int num, const char *msg, const char *path)
{
    printf("liblo server error %d in path %s: %s\n", num, path, msg);
    fflush(stdout);
}

/* catch any incoming messages and display them. returning 1 means that the
* message has not been fully handled and the server should try other methods */
int generic_handler(const char *path, const char *types, lo_arg ** argv,
int argc, lo_message message, void *user_data)
{
    if (!last_address) {
        last_address = lo_address_new_from_copy(lo_message_get_source(message));
    }
    
    //fa_log_region_count("In generic_handler");
    int i;

    printf("msg: %s   ", path);
    for (i = 0; i < argc; i++) {
        printf("arg %d '%c' ", i, types[i]);
        lo_arg_pp((lo_type)types[i], argv[i]);
        printf("   ");
    }
    printf("\n");
    fflush(stdout);
  
    return 1;
}

fa_ptr_t _playback_started(fa_ptr_t context, fa_time_t time)
{
    send_osc_async("/playback/started", "ih", peek_oid(context), (int64_t)fa_time_to_milliseconds(time));
    return NULL;
}

fa_ptr_t _playback_stopped(fa_ptr_t context, fa_time_t time)
{
    oid_t id = peek_oid(context);
    if (remove_playback_semaphore(id)) {
        send_osc_async("/playback/stopped", "ihT", id, (int64_t)fa_time_to_milliseconds(time));
        stop_time_echo();
    }
    return NULL;
}

int play_audio_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data) {
    oid_t id = argv[0]->i;
    oid_t audio_id = argv[1]->i;
    double skip            = argc >= 3 ? (argv[2]->f / 1000.0) : 0.0; // convert from ms to s
    double time            = argc >= 4 ? (argv[3]->f / 1000.0) : 0.0;
    double repeat_interval = argc >= 5 ? (argv[4]->f / 1000.0) : 0.0;
    check_id(id);
    if (!current_audio_stream) {
        send_osc(message, user_data, "/error", "s", "no-audio-stream");
        return 0;
    }
    
    fa_buffer_t buffer = NULL;
    fa_with_lock(audio_files_mutex) {
        buffer = fa_map_dget(wrap_oid(audio_id), audio_files);
    }
    if (buffer) {
        add_playback_semaphore(id, audio_name);
        fa_slog_info("Starting audio playback");
        fa_list_t actions = fa_list_empty();
        fa_ptr_t sample_rate = fa_buffer_get_meta(buffer, fa_string("sample-rate"));
        fa_ptr_t channels    = fa_buffer_get_meta(buffer, fa_string("channels"));
        double max_time = ((double)fa_buffer_size(buffer)) / ((double)sizeof(double) *
            fa_peek_number(sample_rate) * fa_peek_number(channels)); // seconds
        max_time -= skip;
        skip *= fa_peek_number(sample_rate);
        
        // Schedule
        fa_push_list(pair(fa_action_send_retain(audio_name, buffer), fa_milliseconds(0)), actions);
        fa_push_list(pair(fa_action_send(audio_name, fa_from_double(skip)), fa_milliseconds(0)), actions);
        fa_push_list(pair(fa_action_send(audio_name, fa_string("play")), fa_milliseconds(0)), actions);
        if (repeat_interval == 0) {
            //fa_push_list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)), fa_milliseconds(0)), actions);
            //fa_push_list(pair(fa_action_do_with_time(_playback_stopped, wrap_oid(id)), fa_time_from_double(max_time + 0.010)), actions);
        }
        actions = times_to_delta_times(actions); // Convert absolute times to relative times
        
        fa_action_t main_action;
        if (repeat_interval > 0) {
            main_action =
                fa_action_many(
                    list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)), fa_milliseconds(0)),
                         pair(fa_action_while(check_playback_semaphore, wrap_oid(id),
                                              fa_action_repeat(fa_time_from_double(repeat_interval), fa_action_many(actions))),
                              fa_milliseconds(0))));
        } else {
            main_action = fa_action_many(actions);
        }
        
        
        if (time == 0) {
            schedule_relative(fa_now(), main_action, current_audio_stream);
        } else {
            schedule(fa_milliseconds(time), main_action, current_audio_stream);
        }
        start_time_echo();
    }
    return 0;
}

int play_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int id = argv[0]->i;
    lo_blob data = argv[1];
    double time            = argc >= 3 ? (argv[2]->f / 1000.0) : 0.0; // convert from ms to s
    double repeat_interval = argc >= 4 ? (argv[3]->f / 1000.0) : 0.0;
    check_id(id);
    int data_size = lo_blob_datasize(data);
    uint8_t* ptr = lo_blob_dataptr(data);
    if ((data_size % 8) != 0) {
        printf("data_size (%d) not a multiple of 8\n", data_size);
        return 0;
    }
    // fa_log_region_count("BEFORE");
    // fa_log_list_count();
    // fa_log_time_count();
    // fa_log_pair_count();
    // fa_log_pair_left_count();
    // fa_log_string_count();
    
    fa_list_t actions = fa_list_empty();
    int count = data_size / 8;
    printf("%d MIDI entries:\n", count);
    if (count == 0) return 0;
    
    // Collect MIDI entries from the data blob, create MIDI messages and add them to a list
    int32_t max_time = 0;
    for (int i = 0; i < count; i++) {
        uint8_t cmd   = ptr[0];
        uint8_t ch    = ptr[1];
        uint8_t data1 = ptr[2];
        uint8_t data2 = ptr[3];
        int32_t time  = lo_otoh32(*(int32_t *) &ptr[4]);
        max_time = MAX(time, max_time);
        //printf("  %x %x %x %x  %d\n", cmd, ch, data1, data2, time);
        
        fa_midi_message_t msg = fa_midi_message_create_simple(cmd + ch, data1, data2);
        fa_action_t a = fa_action_send(synth_name, msg);
        if (repeat_interval > 0) {
            a = fa_action_if(check_playback_semaphore, wrap_oid(id), a);
        }
        fa_push_list(pair(a, fa_milliseconds(time)), actions);
        ptr += 8;
    }
    
    if (repeat_interval == 0) {
        fa_push_list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)), fa_milliseconds(0)), actions);
        fa_push_list(pair(fa_action_do_with_time(_playback_stopped, wrap_oid(id)), fa_milliseconds(max_time + 10)), actions);
    }
    
    // Convert absolute times to relative times
    actions = times_to_delta_times(actions);
    
    // 
    add_playback_semaphore(id, NULL);
    fa_action_t main_action;
    if (repeat_interval > 0) {
        main_action =
            fa_action_many(
                list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)),
                          fa_milliseconds(0)),
                     pair(fa_action_while(check_playback_semaphore, wrap_oid(id),   
                                          fa_action_repeat(fa_time_from_double(repeat_interval), fa_action_many(actions))),
                          fa_milliseconds(0))));
    } else {
        main_action = fa_action_while(check_playback_semaphore, wrap_oid(id), fa_action_many(actions));
    }
        
    // Send to scheduler
    if (time == 0) {
        schedule_relative(fa_now(), main_action, current_midi_echo_stream);
    } else {
        schedule(fa_time_from_double(time), main_action, current_midi_echo_stream);
    }
    
    start_time_echo();
    
    // fa_log_region_count("AFTER");
    // fa_log_list_count();
    // fa_log_time_count();
    // fa_log_pair_count();
    // fa_log_pair_left_count();
    // fa_log_string_count();
    return 0;
}

int stop_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data) {
    //printf("Stopping playback %d\n", argv[0]->i);
    if (remove_playback_semaphore(argv[0]->i)) {
        send_osc(message, user_data, "/playback/stopped", "ihF", argv[0]->i, fa_clock_milliseconds(current_clock));
        stop_time_echo();
    }
    return 0;
}


int quit_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    done = 1;
    printf("quiting\n\n");
    fflush(stdout);

    return 0;
}

int restart_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    int restart_type = argc ? argv[1]->i : 0;
    
    switch (restart_type) {
    case 0:
        stop_streams();
        remove_all_playback_semaphores();
        start_streams();
        break;
    default:
        fa_log_warning(fa_string("No such restart type!"));
    } 
    return 0;
}

int settings_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    fa_string_t parameter = fa_string(user_data);
    fa_ptr_t value = create_fa_value(types[0], argv[0]);
    fa_slog_info("settings handler: ", parameter, value);
    if (value && current_audio_session) {
        fa_audio_set_parameter(parameter, value, current_audio_session);
    }
    if (value) fa_destroy(value);
    fa_destroy(parameter);
    return 0;
}


/***************************
*   /time
*/

int time_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (current_clock)
        send_osc(message, user_data, "/time", "h", fa_clock_milliseconds(current_clock));
    else
        send_osc(message, user_data, "/time", "N");
    return 0;
}

/***************************
*   /all/devices
*/

int all_devices_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (current_audio_session) {
        fa_list_t audio_devices = fa_audio_all(current_audio_session);
        send_osc(message, user_data, "/audio/devices", "i", fa_list_length(audio_devices));
        int counter = 0;
        fa_for_each (device, audio_devices) {
            //printf("device: %d %s / %s\n", counter, fa_unstring(fa_audio_host_name(device)), fa_unstring(fa_audio_name(device)));
            char *host_name = fa_unstring(fa_audio_host_name(device));
            char *name = fa_unstring(fa_audio_name(device));
            send_osc(message, user_data, "/audio/device", "issiif", counter++, host_name, name,
                fa_audio_input_channels(device), fa_audio_output_channels(device),
                fa_audio_current_sample_rate(device));
            fa_free(host_name);
            fa_free(name);
        }
    } else {
        send_osc(message, user_data, "/error", "s", "no-audio-session");
    }
  
    if (current_midi_session) {
        fa_list_t midi_devices = fa_midi_all(current_midi_session);
        send_osc(message, user_data, "/midi/devices", "i", fa_list_length(midi_devices));
        int counter = 0;
        fa_for_each (device, midi_devices) {
            char *host_name = fa_unstring(fa_midi_host_name(device));
            char *name = fa_unstring(fa_midi_name(device));
            send_osc(message, user_data, "/midi/device", "issii", counter++, host_name, name,
                fa_midi_has_input(device) ? 1 : 0, fa_midi_has_output(device) ? 1 : 0);
            fa_free(host_name);
            fa_free(name);
        }
    } else {
        send_osc(message, user_data, "/error", "s", "no-midi-session");
    }
  
    return 0;
}

/***************************
*   /current/devices
*/

int current_devices_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (current_audio_input_device) {
        char *host_name = fa_unstring(fa_audio_host_name(current_audio_input_device));
        char *name = fa_unstring(fa_audio_name(current_audio_input_device));
        send_osc(message, user_data, "/current/audio/input", "ssiif", host_name, name,
            fa_audio_input_channels(current_audio_input_device),
            fa_audio_output_channels(current_audio_input_device),
            fa_audio_current_sample_rate(current_audio_input_device));
        fa_free(host_name);
        fa_free(name);
    } else {
        send_osc(message, user_data, "/current/audio/input", "N");
    }
  
    if (current_audio_output_device) {
        char *host_name = fa_unstring(fa_audio_host_name(current_audio_output_device));
        char *name = fa_unstring(fa_audio_name(current_audio_output_device));
        send_osc(message, user_data, "/current/audio/output", "issiif", host_name, name,
            fa_audio_input_channels(current_audio_output_device),
            fa_audio_output_channels(current_audio_output_device),
            fa_audio_current_sample_rate(current_audio_output_device));
    } else {
        send_osc(message, user_data, "/current/audio/output", "N");
    }
  
    return 0;
}

/***************************
*   /send/midi
*/

int simple_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    fa_midi_message_t midi_message = fa_midi_message_create_simple(argv[0]->i, argv[1]->i, (argc > 2) ? argv[2]->i : 0);
    fa_action_t action = fa_action_send(synth_name, midi_message);
    schedule_now(action, current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/note
*/

int simple_note_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    int f0 = argv[0]->i;
    int vel = argv[1]->i;
    int ms = argv[2]->i;
    int ch = (argc > 3) ? argv[3]->i : 0;
    if ((ms <= 0) || (vel <= 0) || (ch < 0) || (ch > 15)) return 0;
  
    // fa_action_t action = fa_action_many(list(
    //     fa_pair_create(
    //         fa_action_send(synth_name, fa_midi_message_create_simple(0x90 + ch, f0, vel)), fa_milliseconds(argv[2]->i)
    //             ),
    //     fa_pair_create(
    //         fa_action_send(synth_name, fa_midi_message_create_simple(0x90 + ch, f0, 0)), fa_now()
    //             )));
    // schedule_relative(fa_now(), action, current_midi_echo_stream);
    fa_action_t noteOn  = fa_action_send(synth_name, fa_midi_message_create_simple(0x90 + ch, f0, vel));
    fa_action_t noteOff = fa_action_send(synth_name, fa_midi_message_create_simple(0x90 + ch, f0, 0));
    schedule_now(noteOn, current_midi_echo_stream);
    schedule_relative(fa_milliseconds(argv[2]->i), noteOff, current_midi_echo_stream);
    return 0;
}

int load_audio_file_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    check_id(id);
    fa_string_t file_path = fa_string_from_utf8(&argv[1]->s);
    fa_buffer_t buffer = fa_buffer_read_audio(file_path);
    if (fa_check(buffer)) {
        fa_error_log(NULL, (fa_error_t) buffer); // this destroys buffer (the error)
        fa_destroy(file_path);
        send_osc(message, user_data, "/audio-file/load", "iF", id);
        return 0;
    }
    fa_buffer_set_meta(buffer, fa_string("file_path"), file_path);
    fa_with_lock(audio_files_mutex) {
        audio_files = fa_map_dset(wrap_oid(id), buffer, audio_files);
    }
    fa_ptr_t sample_rate = fa_buffer_get_meta(buffer, fa_string("sample-rate"));
    fa_ptr_t channels    = fa_buffer_get_meta(buffer, fa_string("channels"));
    send_osc(message, user_data, "/audio-file/load", "iTiii", id,
        fa_buffer_size(buffer), safe_peek_i32(sample_rate), safe_peek_i32(channels));
    
    return 0;
}

int close_audio_file_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(audio_files_mutex) {
        fa_buffer_t buffer = fa_map_dget(wrap_oid(id), audio_files);
        if (buffer) {
            fa_destroy(buffer); // this is safe, since the actual destruction is delayed if the buffer is in use
            audio_files = fa_map_dremove(wrap_oid(id), audio_files);
            send_osc(message, user_data, "/audio-file/close", "iT", id);
        } else {
            send_osc(message, user_data, "/audio-file/close", "iF", id);
        }
    }
    return 0;
}

int save_audio_file_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_buffer_t buffer = NULL;
    fa_with_lock(audio_files_mutex) {
        buffer = fa_map_dget(wrap_oid(id), audio_files);
    }
    if (!buffer) {
        fa_fail(fa_format_integral("There is no audio file with ID %zu", id));
        send_osc(message, user_data, "/audio-file/save", "iF", id);
        return 0;
    }
    fa_string_t file_path = fa_buffer_get_meta(buffer, fa_string("file_path"));
    if (argc >= 2) {
        file_path = fa_string(&argv[1]->s);
    } else if (file_path) {
        file_path = fa_copy(file_path);
    } else {
        fa_fail(fa_format_integral("Audio file %zu has no path", id));
        send_osc(message, user_data, "/audio-file/save", "iF", id);
        return 0;
    }
    fa_ptr_t error = fa_buffer_write_audio(file_path, buffer);
    if (error) {
        fa_error_log(NULL, error);
        send_osc(message, user_data, "/audio-file/save", "iF", id);
        fa_destroy(file_path);
        return 0;
    }
    fa_slog_info("Saved audio file at ", file_path);
    fa_buffer_set_meta(buffer, fa_string("file_path"), file_path);
    send_osc(message, user_data, "/audio-file/save", "iT", id);
    return 0;
}

int audio_file_curve_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(audio_files_mutex) {
        fa_buffer_t buffer = fa_map_dget(wrap_oid(id), audio_files);
        if (buffer) {
            lo_blob blob = audio_curve(buffer);
            send_osc(message, user_data, "/audio-file/curve", "ib", id, blob);
            lo_blob_free(blob);
        } else {
            fa_fail(fa_format_integral("There is no audio file with ID %zu", id));
            send_osc(message, user_data, "/audio-file/curve", "iF", id);
        }
    }
    return 0;
}

int audio_file_peak_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(audio_files_mutex) {
        fa_buffer_t buffer = fa_map_dget(wrap_oid(id), audio_files);
        if (buffer) {
            double peak = audio_file_peak(buffer);
            send_osc(message, user_data, "/audio-file/peak", "if", id, peak);
        } else {
            fa_fail(fa_format_integral("There is no audio file with ID %zu", id));
            send_osc(message, user_data, "/audio-file/peak", "iF", id);
        }
    }
    return 0;
}


/***************************
*   /send/main-volume channel volume (0-16383)
*/

int main_volume_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    int vol = argv[1]->i;
    schedule_now(main_volume_action(ch, vol), current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/pan channel pan
*/

int pan_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    float pan = argv[1]->f;
    schedule_now(pan_action(ch, pan), current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/program-change channel bank program
*/

int program_change_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    int bank = argv[1]->i;
    int prog = argv[2]->i;
    fa_action_t action = fa_action_many(list(
        fa_pair_create(program_change_action(ch, prog), fa_now()),
        fa_pair_create(bank_select_action(ch, bank), fa_now())));
    schedule_now(action, current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/pitch-wheel
*/

int pitch_wheel_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    float pitch = argv[1]->f;
    schedule_now(pitch_wheel_action(ch, pitch), current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/sustain-wheel
*/

int sustain_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    bool sustain = argv[1]->i > 0;
    schedule_now(sustain_action(ch, sustain), current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/channel-reset
*/

int channel_reset_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    // Defaults
    int main_volume = 14000;
    float pan = 0.0;
    int bank = 0;
    int program = 0;
    float pitch_wheel = 0.0;
    int sustain = false;
    // If more than one argument provided, override default
    if (argc > 1) {
        main_volume   = argv[1]->i;
        pan           = argv[2]->f;
        bank          = argv[3]->i;
        program       = argv[4]->i;
        pitch_wheel   = argv[5]->f;
        sustain       = (argv[6]->i > 0);
    }
    
    fa_action_t action = fa_action_many(list(
        fa_pair_create(main_volume_action(ch, main_volume), fa_now()),
        fa_pair_create(pan_action(ch, pan), fa_now()),
        fa_pair_create(bank_select_action(ch, bank), fa_now()),
        fa_pair_create(program_change_action(ch, program), fa_now()),
        fa_pair_create(pitch_wheel_action(ch, pitch_wheel), fa_now()),
        fa_pair_create(sustain_action(ch, sustain), fa_now())));

    schedule_relative(fa_now(), action, current_midi_echo_stream);
    return 0;
}


int stats_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    fa_log_region_count(fa_string("Stats:"));
    fa_list_log_count();
    fa_time_log_count();
    fa_pair_log_count();
    fa_pair_left_log_count();
    fa_string_log_count();
    fa_func_ref_log_count();
    fa_action_log_count();
    fa_map_log_count();
    return 0;
}


int volume_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (current_audio_stream) {
        uint32_t channel = (uint32_t) user_data;
        fa_list_t actions = list(pair(fa_action_set(channel + 0, argv[0]->f), fa_now()),
                                 pair(fa_action_set(channel + 1, argv[0]->f), fa_now()));
        schedule_now(fa_action_many(actions), current_audio_stream);
    }
    return 0;
}

int start_recording_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    return 0;
}

int stop_recording_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    return 0;
}