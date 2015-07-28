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
#include "common.h"

#include "server-types.h"
#include "server-globals.h"
#include "server-utils.h"

#define send_osc(m, s, ...) lo_send_from(lo_message_get_source(m), (lo_server)s, LO_TT_IMMEDIATE, __VA_ARGS__)

#define define_handler(name) \
  int name ## _handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)

int done = 0;


void liblo_error(int num, const char *m, const char *path);

int generic_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);
int blob_test_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);

define_handler(quit);
define_handler(restart);

int test_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);
int stats_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int simple_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);
int simple_note_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data);

int time_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int all_devices_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int current_devices_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);

int main_volume_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int pan_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int program_change_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int pitch_wheel_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int sustain_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);
int channel_reset_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data);


int main()
{
    fa_set_log_std();
  
    const char *port = "7770";
  
    /* start a new server on port 7770 */
    lo_server_thread st = lo_server_thread_new_with_proto(port, LO_TCP, liblo_error);
    lo_server s = lo_server_thread_get_server(st);

    /* add method that will match any path and args */
    lo_server_thread_add_method(st, NULL, NULL, generic_handler, NULL);
    
    lo_server_thread_add_method(st, "/blob", "bi", blob_test_handler, NULL);
    
    lo_server_thread_add_method(st, "/stats", "", stats_handler, NULL);

    /* Quit handler */
    lo_server_thread_add_method(st, "/quit", "", quit_handler, NULL);
    lo_server_thread_add_method(st, "/restart", "", restart_handler, NULL);
    lo_server_thread_add_method(st, "/restart", "i", restart_handler, NULL);

    /* add method that will match the path /test with no args */
    lo_server_thread_add_method(st, "/test", "", test_handler, s);
  
    /* Get info */
    lo_server_thread_add_method(st, "/time", "", time_handler, s);
    lo_server_thread_add_method(st, "/all/devices", "", all_devices_handler, s);
    lo_server_thread_add_method(st, "/current/devices", "", current_devices_handler, s);

    /* Send raw midi messages */
    lo_server_thread_add_method(st, "/send/midi", "ii",  simple_midi_handler, s);
    lo_server_thread_add_method(st, "/send/midi", "iii", simple_midi_handler, s);
    /* Send note immediately */
    lo_server_thread_add_method(st, "/send/note", "iii", simple_note_handler, s); // f0, velocity, length (ms)
    lo_server_thread_add_method(st, "/send/note", "iiii", simple_note_handler, s); // f0, velocity, length (ms), channel
    /* Send midi control messages */
    lo_server_thread_add_method(st, "/send/main-volume",    "ii", main_volume_handler, s);
    lo_server_thread_add_method(st, "/send/pan",            "if", pan_handler, s);
    lo_server_thread_add_method(st, "/send/program-change", "iii", program_change_handler, s);
    lo_server_thread_add_method(st, "/send/pitch-wheel",    "if", pitch_wheel_handler, s);
    lo_server_thread_add_method(st, "/send/sustain",        "ii", sustain_handler, s); // ch, sustain (0=up or 1=down)
    lo_server_thread_add_method(st, "/send/channel-reset",  "i", channel_reset_handler, s);
    lo_server_thread_add_method(st, "/send/channel-reset",  "iifiifi", channel_reset_handler, s);

    fa_with_faudio() {
        
        fa_clock_initialize();

#ifdef _WIN32
        synth_name   = fa_string("fluid");
#else
        synth_name   = fa_string("dls");
#endif

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
      
    }

    lo_server_thread_free(st);
    
    fa_log_region_count("At shutdown");
    fa_log_list_count();
    fa_log_time_count();
    fa_log_pair_count();
    fa_log_pair_left_count();
    fa_log_string_count();
    fa_log_func_ref_count();
    fa_log_action_count();

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
int argc, void *data, void *user_data)
{
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

int blob_test_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    lo_blob data = argv[0];
    int id = argv[1]->i;
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
    for (int i = 0; i < count; i++) {
        uint8_t cmd   = ptr[0];
        uint8_t ch    = ptr[1];
        uint8_t data1 = ptr[2];
        uint8_t data2 = ptr[3];
        int32_t time  = lo_otoh32(*(int32_t *) &ptr[4]);
        //printf("  %x %x %x %x  %d\n", cmd, ch, data1, data2, time);
        
        fa_midi_message_t msg = fa_midi_message_create_simple(cmd + ch, data1, data2);
        fa_action_t a = fa_action_send(synth_name, msg);
        fa_push_list(pair(a, fa_milliseconds(time)), actions);
        ptr += 8;
    }
    
    // Convert absolute times to relative times
    actions = times_to_delta_times(actions);
    
    // Send to 
    schedule_relative(fa_now(), fa_action_many(actions), current_midi_echo_stream);
    
    // fa_log_region_count("AFTER");
    // fa_log_list_count();
    // fa_log_time_count();
    // fa_log_pair_count();
    // fa_log_pair_left_count();
    // fa_log_string_count();
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
        start_streams();
        break;
    default:
        fa_dlog_warning(fa_string("No such restart type!"));
    } 
    return 0;
}


int test_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
  
    lo_message message = (lo_message)data;
    lo_address address = lo_message_get_source(message);
    lo_server server = (lo_server)user_data;
    const char *host = lo_address_get_hostname(address);
    const char *port = lo_address_get_port(address);

    //printf("in midi handler\n");

    if (!address) {
        printf("Couldn't get message source, quitting.\n");
        done = 1;
        return 0;
    }

    int r = lo_send_from(address, server, LO_TT_IMMEDIATE, "/answer", "i", 1);
    if (r < 0)
        printf("Error sending back message, socket may have closed.\n");
    else
        printf("Sent %d bytes to %s:%s.\n", r, host, port);
    
  
    fa_action_t chord = fa_action_many(list(
        fa_pair_create(
            fa_action_send(synth_name, fa_midi_message_create_simple(0x90, 64 + ((5 % 12) * 3), 90)), fa_hms(0, 0, 1)
                ),
    fa_pair_create(
        fa_action_send(synth_name, fa_midi_message_create_simple(0x90, 61 + ((5 % 12) * 3), 90)), fa_hms(0, 0, 1)
            )
                ));
    // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
    fa_audio_schedule_relative(fa_hms(0, 0, 0), chord, current_audio_stream);
  
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
    schedule_relative(fa_now(), action, current_midi_echo_stream);
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
  
    fa_action_t action = fa_action_many(list(
        fa_pair_create(
            fa_action_send(synth_name, fa_midi_message_create_simple(0x90 + ch, f0, vel)), fa_milliseconds(argv[2]->i)
                ),
    fa_pair_create(
        fa_action_send(synth_name, fa_midi_message_create_simple(0x90 + ch, f0, 0)), fa_now()
            )
                ));
    schedule_relative(fa_now(), action, current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/main-volume channel volume (0-16383)
*/

int main_volume_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    int vol = argv[1]->i;
    schedule_relative(fa_now(), main_volume_action(ch, vol), current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/pan channel pan
*/

int pan_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    float pan = argv[1]->f;
    schedule_relative(fa_now(), pan_action(ch, pan), current_midi_echo_stream);
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
    schedule_relative(fa_now(), action, current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/pitch-wheel
*/

int pitch_wheel_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    float pitch = argv[1]->f;
    schedule_relative(fa_now(), pitch_wheel_action(ch, pitch), current_midi_echo_stream);
    return 0;
}

/***************************
*   /send/sustain-wheel
*/

int sustain_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    bool sustain = argv[1]->i > 0;
    schedule_relative(fa_now(), sustain_action(ch, sustain), current_midi_echo_stream);
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
    fa_log_region_count("Stats:");
    fa_log_list_count();
    fa_log_time_count();
    fa_log_pair_count();
    fa_log_pair_left_count();
    fa_log_string_count();
    fa_log_func_ref_count();
    fa_log_action_count();
    return 0;
}
