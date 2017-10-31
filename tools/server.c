/*
*  Copyright (C) 2015 Erik Ronström
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <fa/fa.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/string.h>
#include <fa/func_ref.h>
#include <fa/io.h>
#include <fa/option.h>
#include <fa/audio/stream.h>
#include <fa/file_buffer.h>
#include "common.h"

#include <sndfile.h>

#ifdef FA_MP3_IMPORT
#include <mpg123.h>
#endif

#if defined(_WIN32)
#define WIN32 1
#endif

#include "lo/lo.h"

#include "curl/curl.h"

#include "server-types.h"
#include "server-globals.h"
#include "server-utils.h"
#include "server-playback.h"

// #ifndef _WIN32
// #include "server-sleep-mac.h"
// #endif

fa_option_t option_declaration[] = {
    #ifdef _WIN32
    { "s", "soundfont",    "Soundfont path",                  fa_option_native_string,   "FluidR3_GM.sf2" },
    #endif
    { "p", "port",         "Port number",                      fa_option_integral, "7770" },
    { "a", "audio-slots",  "Max simultaneous audio sources",   fa_option_integral, "8"},
    { "h", "default-host", "Default audio host",               fa_option_string, kDefaultAudioHost },
    { "l", "log-file",     "Log file path",                    fa_option_native_string, "" },
    { "v", "verbose",      "Verbose (0 or 1)",                 fa_option_integral, "0" },
    { "n", "noaudio",      "No audio (mainly for debugging)",  fa_option_integral, "0"},
    { "o", "ogg-quality",  "Ogg quality (1-10, default is 9)", fa_option_integral, "9"}
};

#define define_handler(name) \
  int name ## _handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)

int done = 0;


void liblo_error(int num, const char *m, const char *path);

int bundle_start_handler(lo_timetag time, void *user_data);
int bundle_end_handler(void *user_data);

define_handler(generic);
define_handler(fallback);

define_handler(quit);
define_handler(restart_streams);
define_handler(restart_sessions);
define_handler(stop_streams);
define_handler(settings);
define_handler(host_settings);
define_handler(wasapi_hack);
define_handler(stream_direction);

define_handler(echo);
define_handler(test);
define_handler(stats);
define_handler(simple_midi);
define_handler(simple_note);
define_handler(receive_midi);
define_handler(play_midi);
define_handler(play_audio);
define_handler(mix_audio);
define_handler(stop);

define_handler(playback_new);
define_handler(playback_add_midi);
define_handler(playback_add_note);
define_handler(playback_add_audio);
define_handler(playback_start);
define_handler(playback_stop);
define_handler(playback_autostop);
define_handler(playback_status);

define_handler(time);
define_handler(ping);
define_handler(next_id);
define_handler(all_devices);
define_handler(current_devices);
define_handler(stream_info);

define_handler(list_audio_files);
define_handler(load_audio_file);
define_handler(load_raw_audio_file);
define_handler(close_audio_file);
define_handler(save_audio_file);
define_handler(save_raw_audio_file);
define_handler(audio_file_meta);
define_handler(audio_file_curve);
define_handler(audio_file_peak);
define_handler(audio_file_upload);

define_handler(main_volume);
define_handler(pan);
define_handler(program_change);
define_handler(pitch_wheel);
define_handler(sustain);
define_handler(channel_reset);

define_handler(volume);
define_handler(level);

define_handler(start_recording);
define_handler(stop_recording);

define_handler(choose_device);

define_handler(proxy);

define_handler(sleep);

fa_ptr_t _status_callback(fa_ptr_t session);

int init(void *user_data);
int cleanup(void *user_data);

void ctrlc(int sig)
{
    done = 1;
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    
    bool help_only = true;
    fa_string_t log_path = NULL;
    
    char port[14]; // enough to hold all int32 numbers
    fa_with_options(option_declaration, argc, argv, options, args) {
        sprintf(port, "%d", fa_map_get_int32(fa_string("port"), options));
        audio_buffer_signals = fa_map_get_int32(fa_string("audio-slots"), options);
        #ifdef _WIN32
        soundfont_path = fa_map_dget(fa_string("soundfont"), options);
        #endif
        verbose = fa_map_get_int32(fa_string("verbose"), options);
        noaudio = fa_map_get_int32(fa_string("noaudio"), options);
        default_audio_host = fa_map_dget(fa_string("default-host"), options);
        int integer_ogg_quality = fa_map_get_int32(fa_string("ogg-quality"), options);
        if (integer_ogg_quality < 1 || integer_ogg_quality > 10) {
            printf("Invalid ogg quality %d, using 9\n", integer_ogg_quality);
            integer_ogg_quality = 9;
        }
        default_ogg_quality = integer_ogg_quality / 10.0;
        if (verbose) printf("Default ogg quality: %d\n", integer_ogg_quality);
        log_path = fa_map_dget(fa_string("log-file"), options);
        if (fa_string_length(log_path) > 0) {
            fa_set_log_file_and_stdout(log_path);
            char *cpath = fa_unstring(log_path);
            printf("Writing log to %s\n", cpath);
            fa_free(cpath);
        } else {
            printf("No log file set\n");
        }
        help_only = false;
    }
    
    if (help_only) exit(0);
    
    if (audio_buffer_signals < 1 || audio_buffer_signals > kMaxAudioBufferSignals) {
        printf("Invalid number of audio slots (%d), must be between 1 and %d\n", audio_buffer_signals, kMaxAudioBufferSignals);
        exit(4);
    }
    if (verbose) printf("Using %d audio slots\n", audio_buffer_signals);
    
    if (verbose) printf("port = %s\n", port);
    #ifdef _WIN32
    fa_slog_info("Soundfont path: ", soundfont_path);
    #endif

    // Init curl. This MUST be called before any other threads are spawned, even
    // if they are not using libcurl (according to the libcurl documentation).
    if (verbose) printf("Initializing curl\n");
    curl_global_init(CURL_GLOBAL_DEFAULT);
  
    /* start a new server  */
    if (verbose) printf("Starting OSC listener thread...\n");
    lo_server_thread st = lo_server_thread_new_with_proto_and_node(port, LO_TCP, "127.0.0.1", liblo_error);
    if (!st) {
        fa_fail(fa_string_format("Could not start OSC server on port %s, exiting", port));
        curl_global_cleanup();
        exit(3);
    }
    server = lo_server_thread_get_server(st);

    if (verbose) printf("Adding OSC handlers...\n");

    /* add bundle handlers */
    lo_server_add_bundle_handlers(server, bundle_start_handler, bundle_end_handler, NULL);
    
    if (verbose) {
        lo_server_add_method(server, NULL, NULL, generic_handler, server);
    }

    lo_server_add_method(server, "/echo", NULL, echo_handler, server);
    lo_server_add_method(server, "/test", NULL, test_handler, server);
    
    lo_server_add_method(server, "/stats", "", stats_handler, server);

    /* Send raw midi messages */
    lo_server_add_method(server, "/send/midi", "ii",  simple_midi_handler, server);
    lo_server_add_method(server, "/send/midi", "iii", simple_midi_handler, server);
    /* Send note immediately */
    lo_server_add_method(server, "/send/note", "fii", simple_note_handler, server); // f0, velocity, length (ms)
    lo_server_add_method(server, "/send/note", "fiii", simple_note_handler, server); // f0, velocity, length (ms), channel
    
    /* Emulate incoming midi */
    lo_server_add_method(server, "/receive/midi", "ii",   receive_midi_handler, NULL);
    lo_server_add_method(server, "/receive/midi", "iii",  receive_midi_handler, NULL);
    lo_server_add_method(server, "/receive/midi", "iiT",  receive_midi_handler, (void*)true);
    lo_server_add_method(server, "/receive/midi", "iiiT", receive_midi_handler, (void*)true);
    
    /* Quitting, restart */
    lo_server_add_method(server, "/quit", "", quit_handler, server);
    lo_server_add_method(server, "/restart/streams",  "",  restart_streams_handler, server);
    lo_server_add_method(server, "/restart/streams",  "i", restart_streams_handler, server); // id
    lo_server_add_method(server, "/restart/sessions", "",  restart_sessions_handler, server);
    lo_server_add_method(server, "/restart/sessions", "i", restart_sessions_handler, server); // id
    lo_server_add_method(server, "/stop/streams", "",      stop_streams_handler, server);
    lo_server_add_method(server, "/stop/streams", "i",     stop_streams_handler, server); // id
    
    /* Settings */
    lo_server_add_method(server, "/set/latency",           "i",  host_settings_handler, (void*)HOST_SETTINGS_LATENCY);
    lo_server_add_method(server, "/set/latency",           "is", host_settings_handler, (void*)HOST_SETTINGS_LATENCY);
    lo_server_add_method(server, "/set/input-latency",     "i",  host_settings_handler, (void*)HOST_SETTINGS_INPUT_LATENCY);
    lo_server_add_method(server, "/set/input-latency",     "is", host_settings_handler, (void*)HOST_SETTINGS_INPUT_LATENCY);
    lo_server_add_method(server, "/set/output-latency",    "i",  host_settings_handler, (void*)HOST_SETTINGS_OUTPUT_LATENCY);
    lo_server_add_method(server, "/set/output-latency",    "is", host_settings_handler, (void*)HOST_SETTINGS_OUTPUT_LATENCY);
    lo_server_add_method(server, "/set/vector-size",       "i",  host_settings_handler, (void*)HOST_SETTINGS_VECTOR_SIZE);
    lo_server_add_method(server, "/set/vector-size",       "is", host_settings_handler, (void*)HOST_SETTINGS_VECTOR_SIZE);
    lo_server_add_method(server, "/set/sample-rate",        "f",  settings_handler, "sample-rate");
    lo_server_add_method(server, "/set/scheduler-interval", "i",  settings_handler, "scheduler-interval");
    lo_server_add_method(server, "/set/exclusive",          "T",  settings_handler, "exclusive");
    lo_server_add_method(server, "/set/exclusive",          "F",  settings_handler, "exclusive");
    lo_server_add_method(server, "/set/exclusive",          "N",  settings_handler, "exclusive");
    lo_server_add_method(server, "/set/exclusive",          "i",  settings_handler, "exclusive");
    lo_server_add_method(server, "/set/wasapi-hack",        "T",  wasapi_hack_handler, server);
    lo_server_add_method(server, "/set/wasapi-hack",        "F",  wasapi_hack_handler, server);
    lo_server_add_method(server, "/set/wasapi-hack",        "N",  wasapi_hack_handler, server);
    lo_server_add_method(server, "/set/stream-direction",   "s",  stream_direction_handler, server);
    lo_server_add_method(server, "/set/proxy/http",         "sss",  proxy_handler, (void*)0);
    lo_server_add_method(server, "/set/proxy/http",         "s",  proxy_handler, (void*)0);
    lo_server_add_method(server, "/set/proxy/http",         "N",  proxy_handler, (void*)0);
    lo_server_add_method(server, "/set/proxy/https",        "sss",  proxy_handler, (void*)1);
    lo_server_add_method(server, "/set/proxy/https",        "s",  proxy_handler, (void*)1);
    lo_server_add_method(server, "/set/proxy/https",        "N",  proxy_handler, (void*)1);
    //lo_server_add_method(server, "/set/schedule-delay",     "i",  settings_handler, "schedule-delay"); // not implemented

      
    /* Get info */
    lo_server_add_method(server, "/time", NULL, time_handler, server);
    lo_server_add_method(server, "/ping", "", ping_handler, server);
    lo_server_add_method(server, "/next-id", NULL, next_id_handler, server);
    lo_server_add_method(server, "/all/devices", "", all_devices_handler, server);
    lo_server_add_method(server, "/all/devices", "i", all_devices_handler, server);
    lo_server_add_method(server, "/current/devices", "", current_devices_handler, server);
    lo_server_add_method(server, "/stream/info", "", stream_info_handler, server);
    
    /* Playback */
    lo_server_add_method(server, "/playback/new", "i", playback_new_handler, server); // playback id (plid)
    lo_server_add_method(server, "/playback/add/midi", "ifiif", playback_add_midi_handler, server); // plid, time, cmd, ch, data1
    lo_server_add_method(server, "/playback/add/midi", "ifiifi", playback_add_midi_handler, server); // plid, time, cmd, ch, d1, d2
    lo_server_add_method(server, "/playback/add/note", "ififif", playback_add_note_handler, server); // plid, time, ch, pitch, vel, dur
    lo_server_add_method(server, "/playback/add/audio", "ifii", playback_add_audio_handler, server); // plid, time, auid, sl
    lo_server_add_method(server, "/playback/add/audio", "ifiif", playback_add_audio_handler, server); // plid, time, auid, sl, skip
    lo_server_add_method(server, "/playback/add/audio", "ifiiff", playback_add_audio_handler, server); // plid, time, auid, sl, sk, dur
    lo_server_add_method(server, "/playback/status", "i", playback_status_handler, server); // plid
    
    lo_server_add_method(server, "/playback/start", "i", playback_start_handler, server);  // plid (start immediately)
    lo_server_add_method(server, "/playback/start", "if", playback_start_handler, server); // plid, time
    lo_server_add_method(server, "/playback/start/from", "if", playback_start_handler, server); // plid, skip (s)
    lo_server_add_method(server, "/playback/start/from", "iff", playback_start_handler, server); // plid, skip (s), time
    lo_server_add_method(server, "/playback/repeat", "i", playback_start_handler, server); // plid (immediately, auto length)
    lo_server_add_method(server, "/playback/repeat", "if", playback_start_handler, server); // plid, interval (0 = auto) (immediately)
    lo_server_add_method(server, "/playback/repeat", "iff", playback_start_handler, server); // plid, interval (0 = auto), time
    lo_server_add_method(server, "/playback/repeat/from", "iff", playback_start_handler, server); // plid, skip (s), interval, 
    lo_server_add_method(server, "/playback/repeat/from", "ifff", playback_start_handler, server); // plid, skip (s) interval, time
    lo_server_add_method(server, "/playback/stop", "i", playback_stop_handler, server); // plid (stop immediately)
    lo_server_add_method(server, "/playback/stop", "if", playback_stop_handler, server); // plid, time
    lo_server_add_method(server, "/playback/auto-stop", "iT", playback_autostop_handler, server); // plid
    lo_server_add_method(server, "/playback/auto-stop", "iF", playback_autostop_handler, server); // plid
    
    /* Deprecated */
    //  /play/audio  id,  audio id,  slot,  skip (ms),  start-time (ms),  repeat-interval (ms)
    //  /play/midi   id,  data,  start-time (ms),  repeat-interval (ms)
    //  /stop        id
    lo_server_add_method(server, "/play/audio", "ii", play_audio_handler, server);
    lo_server_add_method(server, "/play/audio", "iii", play_audio_handler, server);
    lo_server_add_method(server, "/play/audio", "iiif", play_audio_handler, server);
    lo_server_add_method(server, "/play/audio", "iiiff", play_audio_handler, server);
    lo_server_add_method(server, "/play/audio", "iiifff", play_audio_handler, server);
    lo_server_add_method(server, "/play/midi", "ib", play_midi_handler, server);
    lo_server_add_method(server, "/play/midi", "ibf", play_midi_handler, server);
    lo_server_add_method(server, "/play/midi", "ibff", play_midi_handler, server);
    lo_server_add_method(server, "/play/midi", "ibffT", play_midi_handler, server); // auto-stop, same as default
    lo_server_add_method(server, "/play/midi", "ibffF", play_midi_handler, server); // no auto-stop

    lo_server_add_method(server, "/stop", "i", stop_handler, server);
    
    /* Audio play_buffer settings   (slot, value) */
    lo_server_add_method(server, "/audio/volume", "if", mix_audio_handler, "volume");
    lo_server_add_method(server, "/audio/pan",    "if", mix_audio_handler, "pan");

    /* Audio files handling */
    lo_server_add_method(server, "/audio-file/list",      "i", list_audio_files_handler, server);  // id
    lo_server_add_method(server, "/audio-file/load",      "is", load_audio_file_handler, server);  // audio id, path
    lo_server_add_method(server, "/audio-file/load",      "isi", load_audio_file_handler, server);  // a_id, path, max_size
    lo_server_add_method(server, "/audio-file/load",      "isiT", load_audio_file_handler, server); // a_id, p, m_s, crop
    lo_server_add_method(server, "/audio-file/load",      "isiN", load_audio_file_handler, server);
    lo_server_add_method(server, "/audio-file/load",      "isiF", load_audio_file_handler, server);
    lo_server_add_method(server, "/audio-file/load/raw",  "isii", load_raw_audio_file_handler, server);  // a id, path, sr, ch
    lo_server_add_method(server, "/audio-file/load/raw",  "isiif", load_raw_audio_file_handler, server);  // + latency (ms)
    lo_server_add_method(server, "/audio-file/close",     "i",  close_audio_file_handler, server); // audio id
    lo_server_add_method(server, "/audio-file/meta",      "iis", audio_file_meta_handler, server); // id, audio id, key
    lo_server_add_method(server, "/audio-file/meta",      "iisN", audio_file_meta_handler, server); // id, audio id, key
    lo_server_add_method(server, "/audio-file/meta",      "iiss", audio_file_meta_handler, server); // id, audio id, key, value
    lo_server_add_method(server, "/audio-file/save/wav",  "iis", save_audio_file_handler, server);  // id, audio id, path
    lo_server_add_method(server, "/audio-file/save/aiff", "iis", save_audio_file_handler, server);  // id, audio id, path
    lo_server_add_method(server, "/audio-file/save/ogg",  "iis", save_audio_file_handler, server);  // id, audio id, path
    lo_server_add_method(server, "/audio-file/save/ogg",  "iisf", save_audio_file_handler, server);  // id, audio id, path, quality
    lo_server_add_method(server, "/audio-file/save/mp3",  "iis", save_audio_file_handler, server);  // id, audio id, path
    lo_server_add_method(server, "/audio-file/save/mp3",  "iisi", save_audio_file_handler, server);  // id, audio id, path, bitrate
    lo_server_add_method(server, "/audio-file/save/raw",  "iis", save_audio_file_handler, server);  // id, audio id, path
    lo_server_add_method(server, "/audio-file/curve",     "i",  audio_file_curve_handler, server); // audio id
    lo_server_add_method(server, "/audio-file/peak",      "i",  audio_file_peak_handler, server);  // audio id
    // id, audio id, url, cookies, [from_time (ms), to_time (ms), ogg_dump_filename]
    lo_server_add_method(server, "/audio-file/upload",   "iiss",  audio_file_upload_handler, server);
    lo_server_add_method(server, "/audio-file/upload",   "iissff",  audio_file_upload_handler, server);
    lo_server_add_method(server, "/audio-file/upload",   "iissffs",  audio_file_upload_handler, server);
    
    /* Send midi control messages */
    lo_server_add_method(server, "/send/main-volume",    "ii", main_volume_handler, server);
    lo_server_add_method(server, "/send/pan",            "if", pan_handler, server);
    lo_server_add_method(server, "/send/program-change", "iii", program_change_handler, server);
    lo_server_add_method(server, "/send/pitch-wheel",    "if", pitch_wheel_handler, server);
    lo_server_add_method(server, "/send/sustain",        "ii", sustain_handler, server); // ch, sustain (0=up or 1=down)
    lo_server_add_method(server, "/send/channel-reset",  "i", channel_reset_handler, server);
    lo_server_add_method(server, "/send/channel-reset",  "iifiifi", channel_reset_handler, server);
    
    /* Volume control */
    lo_server_add_method(server, "/volume/synth",   "f", volume_handler, (void*)((uint32_t)kSynthLeft));
    lo_server_add_method(server, "/volume/audio",   "f", volume_handler, (void*)((uint32_t)kAudioLeft));
    lo_server_add_method(server, "/volume/monitor", "f", volume_handler, (void*)((uint32_t)kMonitorLeft));
    
    lo_server_add_method(server, "/level/start", "", level_handler, (void*)true);
    lo_server_add_method(server, "/level/stop", "", level_handler, (void*)false);
    
    // id (not currently used), filename, url, cookies, rel-start-time (s), max-length (s)
    lo_server_add_method(server, "/recording/start", "isssff", start_recording_handler, server);
    lo_server_add_method(server, "/recording/stop",  "i", stop_recording_handler, server);
    
    lo_server_add_method(server, "/choose/audio/input",  "",    choose_device_handler, (void*)CHOOSE_AUDIO_INPUT_NONE);
    lo_server_add_method(server, "/choose/audio/input",  "N",   choose_device_handler, (void*)CHOOSE_AUDIO_INPUT_NONE);
    lo_server_add_method(server, "/choose/audio/input",  "T",   choose_device_handler, (void*)CHOOSE_AUDIO_INPUT_DEFAULT);
    lo_server_add_method(server, "/choose/audio/input",  "ss",  choose_device_handler, (void*)CHOOSE_AUDIO_INPUT_DEVICE);
    lo_server_add_method(server, "/choose/audio/output", "",    choose_device_handler, (void*)CHOOSE_AUDIO_OUTPUT_NONE);
    lo_server_add_method(server, "/choose/audio/output", "N",   choose_device_handler, (void*)CHOOSE_AUDIO_OUTPUT_NONE);
    lo_server_add_method(server, "/choose/audio/output", "T",   choose_device_handler, (void*)CHOOSE_AUDIO_OUTPUT_DEFAULT);
    lo_server_add_method(server, "/choose/audio/output", "ss",  choose_device_handler, (void*)CHOOSE_AUDIO_OUTPUT_DEVICE);
    lo_server_add_method(server, "/choose/midi/input",   "",    choose_device_handler, (void*)CHOOSE_MIDI_INPUT_NONE);
    lo_server_add_method(server, "/choose/midi/input",   "T",   choose_device_handler, (void*)CHOOSE_MIDI_INPUT_ALL);
    lo_server_add_method(server, "/choose/midi/input",   "ss",  choose_device_handler, (void*)CHOOSE_MIDI_INPUT_DEVICE);
    lo_server_add_method(server, "/choose/midi/playback", "",   choose_device_handler, (void*)CHOOSE_MIDI_PLAYBACK_NONE);
    lo_server_add_method(server, "/choose/midi/playback", "N",  choose_device_handler, (void*)CHOOSE_MIDI_PLAYBACK_NONE);
    lo_server_add_method(server, "/choose/midi/playback", "T",  choose_device_handler, (void*)CHOOSE_MIDI_PLAYBACK_AUDIO);
    lo_server_add_method(server, "/choose/midi/playback", "ss", choose_device_handler, (void*)CHOOSE_MIDI_PLAYBACK_DEVICE);
    lo_server_add_method(server, "/choose/midi/echo",    "",    choose_device_handler, (void*)CHOOSE_MIDI_ECHO_NONE);
    lo_server_add_method(server, "/choose/midi/echo",    "N",   choose_device_handler, (void*)CHOOSE_MIDI_ECHO_NONE);
    lo_server_add_method(server, "/choose/midi/echo",    "T",   choose_device_handler, (void*)CHOOSE_MIDI_ECHO_PLAYBACK);
    lo_server_add_method(server, "/choose/midi/echo",    "F",   choose_device_handler, (void*)CHOOSE_MIDI_ECHO_AUDIO);
    lo_server_add_method(server, "/choose/midi/echo",    "ss",  choose_device_handler, (void*)CHOOSE_MIDI_ECHO_DEVICE);
    lo_server_add_method(server, "/choose/midi/echo/channel", "", choose_device_handler, (void*)CHOOSE_MIDI_ECHO_CHANNEL);
    lo_server_add_method(server, "/choose/midi/echo/channel", "N", choose_device_handler, (void*)CHOOSE_MIDI_ECHO_CHANNEL);
    lo_server_add_method(server, "/choose/midi/echo/channel", "i", choose_device_handler, (void*)CHOOSE_MIDI_ECHO_CHANNEL);
    
    lo_server_add_method(server, "/sleep", "i", sleep_handler, server);

    /* add method that will match any path and args */
    lo_server_add_method(server, NULL, NULL, fallback_handler, server);

    printf("Starting FAudio...\n");

    // start_streams(); // server-utils.h

    lo_server_thread_add_functions(st, init, cleanup, port);
    lo_server_thread_start(st);

    signal(SIGINT, ctrlc);

    while (!done) {
#ifdef WIN32
        Sleep(1);
#else
        usleep(1000);
#endif
    }

    fa_slog_info("Destroying lo server thread...");
    lo_server_thread_free(st);
    
    fa_slog_info("CURL cleanup...");
    curl_global_cleanup(); // This must be run AFTER all other threads have exited!

    return 0;
}

int init(void *user_data)
{
    char *port = (char*)user_data;

    init_globals();

    if (noaudio) {
        fa_slog_info("Running audio server with audio disabled");
        #ifdef FA_MP3_IMPORT
        // This is usually done by fa_initialize, so do it here instead.
        // TODO: less ugly solution
        mpg123_init();
        #endif
        void fa_clock_initialize();
        fa_clock_initialize();
        current_clock = fa_clock_standard();
    } else {
        fa_initialize();
        void fa_clock_initialize();
        fa_clock_initialize();
        start_sessions();
    }
    
    fa_inform(fa_dappend(fa_string("Listening on TCP port "), fa_string_from_utf8(port)));
    return 0;
}

int cleanup(void *user_data)
{
    if (!noaudio) {
        if (verbose) fa_slog_info("Stopping streams...");
        stop_streams();
        if (verbose) fa_slog_info("Stopping sessions...");
        stop_sessions();
    }
    if (verbose) fa_slog_info("Destroying globals...");
    destroy_globals();
    
    if (!noaudio) fa_terminate();
    
    if (verbose) {
        fa_log_region_count("At cleanup");
        fa_list_log_count();
        fa_time_log_count();
        fa_pair_log_count();
        fa_pair_left_log_count();
        fa_string_log_count();
        fa_func_ref_log_count();
        fa_action_log_count();
        fflush(stdout);
    }
    return 0;
}

void liblo_error(int num, const char *msg, const char *path)
{
    // printf("liblo error %d in path %s: %s\n", num, path, msg);
    if (path) {
        fa_warn(fa_string_format("liblo error %d in path %s: %s", num, path, msg));
    } else {
        fa_warn(fa_string_format("liblo error %d: %s", num, msg));
    }
    fflush(stdout);
}

int bundle_start_handler(lo_timetag time, void *user_data) {
    if (in_bundle) {
        fa_warn(fa_string("Cannot currently handle nested bundles, sent bundles will be flattened"));
    }
    in_bundle++;
    bundle_time = fa_time_from_double(timetag_to_double(time));
    //printf("Time: %f\n", timetag_to_double(time));
    return 0;
}

int bundle_end_handler(void *user_data) {
    assert(in_bundle);
    in_bundle--;
    if (in_bundle == 0) {
        if (!fa_list_is_empty(bundle_actions)) {
            //fa_slog_info("end handler: ", bundle_actions);
            fa_action_t action = fa_action_many(times_to_delta_times(bundle_actions)); // preserve original order
            if (fa_time_is_zero(bundle_time)) {
                if (fa_action_is_flat(action)) {
                    //fa_slog_info("  flat -> now");
                    do_schedule_now(action, bundle_stream);
                } else {
                    //fa_slog_info("  non-flat -> relative");
                    do_schedule_relative(sched_delay, action, bundle_stream);
                }
                fa_destroy(bundle_time);
            } else {
                //fa_slog_info("  absolute time");
                do_schedule(bundle_time, action, bundle_stream);
            }
            bundle_actions = fa_list_empty();
        }
        bundle_stream = NULL;
        bundle_time = NULL;
    }
    return 0;
}

/* catch any incoming messages and display them. returning 1 means that the
* message has not been fully handled and the server should try other methods */
int generic_handler(const char *path, const char *types, lo_arg ** argv,
int argc, lo_message message, void *user_data)
{
    char t[50];
    time_t rawtime;
    time(&rawtime);
    fa_let(tm, localtime(&rawtime)) {
        strftime(t, 50, "%Y-%m-%d %H:%M:%S%z", tm);
    }
    printf("%s  %s   ", t, path);
    for (int i = 0; i < argc; i++) {
        printf("arg %d '%c' ", i, types[i]);
        lo_arg_pp((lo_type)types[i], argv[i]);
        printf("   ");
    }
    printf("\n");
    fflush(stdout);
  
    return 1;
}

/* fallback handler, if no other handler matched */
int fallback_handler(const char *path, const char *types, lo_arg ** argv,
int argc, lo_message message, void *user_data)
{
    char t[50];
    time_t rawtime;
    time(&rawtime);
    fa_let(tm, localtime(&rawtime)) {
        strftime(t, 50, "%Y-%m-%d %H:%M:%S%z", tm);
    }
    printf("%s  Unrecognized message: %s   ", t, path);
    for (int i = 0; i < argc; i++) {
        printf("arg %d '%c' ", i, types[i]);
        lo_arg_pp((lo_type)types[i], argv[i]);
        printf("   ");
    }
    printf("\n");
    fflush(stdout);
  
    return 0;
}

int echo_handler(const char *path, const char *types, lo_arg ** argv,
int argc, lo_message message, void *user_data)
{
    fa_with_lock(osc_mutex) {
        lo_address a = lo_message_get_source(message);
        int r = lo_send_message_from(a, (lo_server)user_data, path, message);
        if (r < 0) {
            fa_string_t errstr = fa_string_from_utf8(lo_address_errstr(a));
            fa_fail(fa_dappend(fa_format_integral("Could not echo message: %d ", lo_address_errno(a)), errstr));
        }
    }
    return 0;
}

int test_handler(const char *path, const char *types, lo_arg ** argv,
int argc, lo_message message, void *user_data)
{
    send_osc(message, user_data, "/test", "");
    return 0;
}


int play_audio_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data) {
    oid_t id               = argv[0]->i;
    oid_t audio_id         = argv[1]->i;
    int slot               = argc >= 3 ? argv[2]->i : 0;
    double skip            = argc >= 4 ? (argv[3]->f / 1000.0) : 0.0; // convert from ms to s
    double time            = argc >= 5 ? (argv[4]->f / 1000.0) : 0.0;
    double repeat_interval = argc >= 6 ? (argv[5]->f / 1000.0) : 0.0;
    check_id(id, message, user_data);

    if (noaudio) {
        fa_slog_info("Audio disabled, ignoring play_audio");
        return 0;
    }

    if (!current_audio_stream) {
        send_osc(message, user_data, "/error", "is", id, "no-audio-stream");
        return 0;
    }
    if (slot < 0 || slot >= kMaxAudioBufferSignals) {
        fa_fail(fa_format_integral("Bad audio slot %d", slot));
        fa_inform(fa_format_integral("Supported slots: 0-%d", kMaxAudioBufferSignals-1));
        send_osc(message, user_data, "/error", "isii", id, "bad-audio-slot", slot, kMaxAudioBufferSignals);
        return 0;
    }
    
    fa_ptr_t buffer = NULL;
    fa_with_lock(audio_files_mutex) {
        buffer = fa_map_dget(wrap_oid(audio_id), audio_files);
    }
    if (buffer) {
        add_playback_semaphore(id, audio_name, slot);
        if (verbose) fa_inform(fa_format_integral("Starting audio playback on slot %d", slot));
        fa_list_t actions = fa_list_empty();
        double sample_rate = fa_peek_number(fa_get_meta(buffer, fa_string("sample-rate")));
        size_t frames = fa_peek_integer(fa_get_meta(buffer, fa_string("frames")));
        double max_time = (double) frames / sample_rate; // seconds
        max_time -= skip;
        skip *= sample_rate;
        
        if (max_time <= 0) {
            send_osc(message, user_data, "/error", "isi", id, "skipping-past-end", audio_id);
            return 0;
        }
        
        // Schedule
        fa_push_list(pair(fa_action_send_retain(audio_name, pair(fa_i16(slot), buffer)), fa_milliseconds(0)), actions);
        fa_push_list(pair(fa_action_send(audio_name, pair(fa_i16(slot), fa_from_int64(skip))), fa_milliseconds(0)), actions);
        fa_push_list(pair(fa_action_send(audio_name, pair(fa_i16(slot), fa_string("play"))), fa_milliseconds(0)), actions);
        if (repeat_interval == 0) {
            fa_push_list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)), fa_milliseconds(0)), actions);
            fa_push_list(pair(fa_action_do_with_time(_playback_stopped, wrap_oid(id)), fa_time_from_double(max_time + 0.010)), actions);
        }
        actions = times_to_delta_times(actions); // Convert absolute times to relative times
        
        fa_action_t main_action;
        if (repeat_interval > 0) {
            main_action =
                fa_action_many(
                    list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)), fa_milliseconds(0)),
                         pair(fa_action_while(check_playback_semaphore, wrap_oid(id),
                                              fa_action_repeat(fa_time_from_double(repeat_interval), 0, fa_action_many(actions))),
                              fa_milliseconds(0))));
        } else {
            main_action = fa_action_many(actions);
        }
        
        if (time == 0 && !in_bundle) {
            buffer_hint(buffer, skip);
            schedule_relative(sched_delay, main_action, current_audio_stream);
        } else if (time < 0 || (time == 0 && in_bundle)) {
            buffer_hint(buffer, skip);
            schedule_relative(fa_milliseconds(0), main_action, current_audio_stream);
        } else {
            // TODO: send hint as action
            schedule(fa_time_from_double(time), main_action, current_audio_stream);
        }
        
        start_time_echo();
    } else {
        send_osc(message, user_data, "/error", "isi", id, "no-such-audio-file", audio_id);
    }
    return 0;
}

int mix_audio_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data) {
    fa_string_t parameter = fa_string_from_utf8(user_data);
    fa_ptr_t slot = create_fa_value(types[0], argv[0]);
    fa_ptr_t value = create_fa_value(types[1], argv[1]);
    fa_action_t action = fa_action_send(audio_name, pair(slot, pair(parameter, value)));
    schedule_now(action, current_audio_stream);
    return 0;
}

int play_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int id = argv[0]->i;
    lo_blob data = argv[1];
    // Allowed time values:
    // 0     "soon" = default sched_delay
    // < 0   "now"  = immediately
    // > 0   absolute time
    double time            = argc >= 3 ? (argv[2]->f / 1000.0) : 0.0; // convert from ms to s
    double repeat_interval = argc >= 4 ? (argv[3]->f / 1000.0) : 0.0;
    bool auto_stop = (strlen(types) >= 5 && types[4] == 'F') ? false : true;
    time = time + 0; // ?
    check_id(id, message, user_data);
    int data_size = lo_blob_datasize(data);
    uint8_t* ptr = lo_blob_dataptr(data);
    if (data_size == 0 && repeat_interval > 0) {
        fa_slog_error("/play/midi: cannot repeat zero midi events!");
        send_osc(message, user_data, "/error", "is", id, "cannot-repeat-zero-events");
        return 0;
    }
    if (data_size == 0 && (repeat_interval > 0 || auto_stop)) {
        fa_slog_warning("/play/midi: no events to play");
        //return 0;
    }
    if ((data_size % 12) != 0) {
        fa_fail(fa_format_integral("data_size (%d) not a multiple of 12 in /play/midi\n", data_size));
        send_osc(message, user_data, "/error", "is", id, "bad-midi-data");
        return 0;
    }
    if (!current_midi_playback_stream) {
        fa_slog_warning("/play/midi: cannot play, there is no playback stream");
        send_osc(message, user_data, "/error", "is", id, "no-playback-stream");
        return 0;
    }

    fa_list_t actions = fa_list_empty();
    int count = data_size / 12;
    fa_inform(fa_format_integral("%d MIDI entries\n", count));
    
    typedef union {
        int32_t i;
        float f;
        char c;
        uint32_t nl;
    } lo_pcast32;
    
    lo_pcast32 val32;

    // Collect MIDI entries from the data blob, create MIDI messages and add them to a list
    int32_t max_time = 0;
    for (int i = 0; i < count; i++) {
        uint8_t cmd   = ptr[0];
        uint8_t ch    = ptr[1];
        uint8_t data1 = ptr[2];
        uint8_t data2 = ptr[3];
        int32_t time  = lo_otoh32(*(int32_t *) &ptr[4]);
        val32.nl = lo_otoh32(*(int32_t *) &ptr[8]);
        float f0  = val32.f;
        max_time = MAX(time, max_time);
        //printf("  %x %x %x %x  %d   %f\n", cmd, ch, data1, data2, time, f0);

        fa_action_t a;

        if (cmd == 0x80 || cmd == 0x90) {
            int pitch = f0; // truncate
            int cents = round((double)(f0 - pitch) * (double)100.0);
            //printf("%x %x %x\n", pitch, data2, cents);
            a = fa_action_send(synth_name, fa_midi_message_create_extended(cmd + ch, pitch, data2, (uint8_t)cents));
        } else {
            a = fa_action_send(synth_name, fa_midi_message_create_simple(cmd + ch, data1, data2));
        }
        if (repeat_interval > 0) {
            a = fa_action_if(check_playback_semaphore, wrap_oid(id), a);
        }
        fa_push_list(pair(a, fa_milliseconds(time)), actions);
        ptr += 12;
    }

    if (repeat_interval == 0) {
        fa_push_list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)), fa_milliseconds(0)), actions);
        if (auto_stop) {
            max_time += 10; // add 10 ms margin
            fa_push_list(pair(fa_action_do_with_time(_playback_stopped, wrap_oid(id)), fa_milliseconds(max_time)), actions);
        }
    }

    // Convert absolute times to relative times
    actions = times_to_delta_times(actions);
    
    //
    add_playback_semaphore(id, NULL, 0);
    fa_action_t main_action;
    if (repeat_interval > 0) {
        main_action =
            fa_action_many(
                list(pair(fa_action_do_with_time(_playback_started, wrap_oid(id)),
                          fa_milliseconds(0)),
                     pair(fa_action_while(check_playback_semaphore, wrap_oid(id),
                                          fa_action_repeat(fa_time_from_double(repeat_interval), 0, fa_action_many(actions))),
                          fa_milliseconds(0))));
    } else {
        main_action = fa_action_while(check_playback_semaphore, wrap_oid(id), fa_action_many(actions));
    }

    // printf("Before deep destroy\n");
    // fa_action_log_count();
    // fa_deep_destroy_always(main_action);
    // printf("After deep destroy\n");
    // fa_action_log_count();
    // return 0;

    // Send to scheduler
    if (time == 0 && !in_bundle) {
        schedule_relative(sched_delay, main_action, current_midi_playback_stream);
    } else if (time < 0 || (time == 0 && in_bundle)) {
        schedule_relative(fa_milliseconds(0), main_action, current_midi_playback_stream);
    } else {
        schedule(fa_time_from_double(time), main_action, current_midi_playback_stream);
    }

    start_time_echo();

    return 0;
}

int stop_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data) {
    //fa_slog_info("stop_handler");
    
    // Delegate to the playback_stop_handler if there is a matching playback id
    // Not very elegant perhaps, but it lets the uses call /stop with any id
    bool done = false;
    fa_with_lock(playback_data_mutex) {
        playback_data_t playback = fa_map_dget(wrap_oid(argv[0]->i), playback_data);
        if (playback) {
            playback_stop(argv[0]->i, message, user_data);
            done = true; // Can't return here, lock wouldn't be released
        }
    }
    if (done) return 0;
    
    if (remove_playback_semaphore(argv[0]->i)) {
        // printf("Stopping play id %d\n", argv[0]->i);
        fa_time_t now = fa_clock_time(current_clock);
        send_osc(message, user_data, "/playback/stopped", "itF", argv[0]->i, timetag_from_time(now));
        fa_destroy(now);
        //printf("calling stop_time_echo from stop_handler (%d)\n", argv[0]->i);
        stop_time_echo();
        if (current_midi_playback_stream) {
            // Stop sounding notes as fast as possible...
            do_schedule_now(all_notes_off_action(), current_midi_playback_stream);
            // ... but also schedule a note-off for notes that may
            // already have been sent to the audio thread
            do_schedule_relative(sched_delay, all_notes_off_action(), current_midi_playback_stream);
        }
    } else {
        fa_warn(fa_format_integral("Could not remove semaphore for id %d", argv[0]->i));
    }
    return 0;
}


int quit_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    done = 1;
    fa_slog_info("Quitting");

    return 0;
}

int settings_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    fa_string_t parameter = fa_string_from_utf8(user_data);
    fa_ptr_t value = create_fa_value(types[0], argv[0]);
    if (!value) value = fa_from_bool(false);
    session_settings = fa_map_dset(parameter, value, session_settings);
    return 0;
}


int host_settings_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    host_setting_t parameter = (int)user_data;
    fa_ptr_t value = create_fa_value(types[0], argv[0]);
    fa_ptr_t host = (argc > 1) ? fa_string_from_utf8(&argv[1]->s) : fa_string("");
    // fa_slog_info("host_settings handler: ", fa_from_int16(parameter), host, value);
    switch (parameter) {
    case HOST_SETTINGS_LATENCY:
        host_input_latency  = fa_map_dset(fa_copy(host), fa_copy(value), host_input_latency);
        host_output_latency = fa_map_dset(host, value, host_output_latency);
        break;
    case HOST_SETTINGS_INPUT_LATENCY:
        host_input_latency  = fa_map_dset(host, value, host_input_latency);
        break;
    case HOST_SETTINGS_OUTPUT_LATENCY:
        host_output_latency = fa_map_dset(host, value, host_output_latency);
        break;
    case HOST_SETTINGS_VECTOR_SIZE:
        host_vector_size    = fa_map_dset(host, value, host_vector_size);
        break;
    }
    
    // fa_slog_info("Current settings:");
    // fa_slog_info("Input latency:   ", host_input_latency);
    // fa_slog_info("Output latency:  ", host_output_latency);
    // fa_slog_info("Vector size:     ", host_vector_size);
    
    return 0;
}



/***************************
*   /time
*/

int time_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    // Save last_address, so that async osc can be used
    // TODO: move this to a dedicated handler
    if (last_address) {
        lo_address_free(last_address);
    }
    last_address = lo_address_new_from_copy(lo_message_get_source(message));
    
    
    if (current_clock) {
        fa_time_t time = fa_clock_time(current_clock);
        if (argc > 0) {
            char t[3] = "t_\0";
            t[1] = types[0];
            if (t[1] == 's') {
                send_osc(message, user_data, "/time", t, timetag_from_time(time), argv[0]);
            } else {
                send_osc(message, user_data, "/time", t, timetag_from_time(time), *argv[0]);
            }
        } else {
            send_osc(message, user_data, "/time", "t", timetag_from_time(time));
        }
        fa_destroy(time);
    } else {
        send_osc(message, user_data, "/time", "N");
    }
    return 0;
}

/***************************
*   /time
*/

int ping_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    send_osc(message, user_data, "/ping", "i", ++ping_counter);
    return 0;
}

/***************************
*   /next-id
*/

int next_id_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    // Save last_address, so that async osc can be used
    // TODO: move this to a dedicated handler
    if (last_address) {
        lo_address_free(last_address);
    }
    last_address = lo_address_new_from_copy(lo_message_get_source(message));
    
    send_osc(message, user_data, "/next-id", "i", last_used_id + 1);
    return 0;
}

/***************************
*   /all/devices
*/

void send_all_devices(int id, lo_message message, void *user_data)
{
    bool errors = false;

    if (!noaudio) {
        if (current_audio_session) {
            fa_list_t audio_devices = fa_audio_all(current_audio_session);
            send_osc(message, user_data, "/audio/devices", "i", fa_list_length(audio_devices));
            int counter = 0;
            fa_for_each (device, audio_devices) {
                //printf("device: %d %s / %s\n", counter, fa_dunstring(fa_audio_host_name(device)), fa_unstring(fa_audio_name(device)));
                char *host_name = fa_dunstring(fa_audio_host_name(device));
                char *name = fa_dunstring(fa_audio_name(device));
                send_osc(message, user_data, "/audio/device", "issiif", counter++, host_name, name,
                    fa_audio_input_channels(device), fa_audio_output_channels(device),
                    fa_audio_default_sample_rate(device));
                fa_free(host_name);
                fa_free(name);
            }
            fa_destroy(audio_devices);
        } else {
            fa_slog_warning("No audio session!");
            send_osc(message, user_data, "/audio-devices", "i", 0);
            errors = true;
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
            fa_destroy(midi_devices);
        } else {
            fa_slog_warning("No MIDI session!");
            send_osc(message, user_data, "/midi/devices", "i", 0);
            errors = true;
        }
    }
    
    if (id >= 0) {
        if (errors) {
            send_osc(message, user_data, "/all/devices", "iF", id);
        } else {
            send_osc(message, user_data, "/all/devices", "iT", id);
        }
    } else {
        if (errors) {
            send_osc(message, user_data, "/all/devices", "NF");
        } else {
            send_osc(message, user_data, "/all/devices", "NT");
        }
    }
}

int all_devices_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (noaudio) return 0;
    int id = argc > 0 ? argv[0]->i : -1;
    send_all_devices(id, message, user_data);
    return 0;
}

/***************************
*   /current/devices
*/

void send_current_devices(lo_message message, void *user_data)
{
    if (current_audio_input_device) {
        char *host_name = fa_dunstring(fa_audio_host_name(current_audio_input_device));
        char *name = fa_dunstring(fa_audio_name(current_audio_input_device));
        send_osc(message, user_data, "/current/audio/input", "ssiif", host_name, name,
            fa_audio_input_channels(current_audio_input_device),
            fa_audio_output_channels(current_audio_input_device),
            fa_audio_default_sample_rate(current_audio_input_device));
        fa_free(host_name);
        fa_free(name);
    } else {
        send_osc(message, user_data, "/current/audio/input", "N");
    }
  
    if (current_audio_output_device) {
        char *host_name = fa_dunstring(fa_audio_host_name(current_audio_output_device));
        char *name = fa_dunstring(fa_audio_name(current_audio_output_device));
        send_osc(message, user_data, "/current/audio/output", "ssiif", host_name, name,
            fa_audio_input_channels(current_audio_output_device),
            fa_audio_output_channels(current_audio_output_device),
            fa_audio_default_sample_rate(current_audio_output_device));
        fa_free(host_name);
        fa_free(name);
    } else {
        send_osc(message, user_data, "/current/audio/output", "N");
    }

    // TODO: MIDI inputs
    
    switch (current_midi_playback) {
    case FA_MIDI_NO_OUTPUT:
        send_osc(message, user_data, "/current/midi/playback", "N");
        break;
    case FA_MIDI_TO_AUDIO:
        send_osc(message, user_data, "/current/midi/playback", "T");
        break;
    case FA_MIDI_TO_DEVICE:
        assert(current_midi_playback_device && "current_midi_playback_device is NULL in current_devices_handler");
        char *host_name = fa_dunstring(fa_midi_host_name(current_midi_playback_device));
        char *name = fa_dunstring(fa_midi_name(current_midi_playback_device));
        send_osc(message, user_data, "/current/midi/playback", "ssii", host_name, name,
            fa_midi_has_input(current_midi_playback_device) ? 1 : 0,
            fa_midi_has_output(current_midi_playback_device) ? 1 : 0);
        fa_free(host_name);
        fa_free(name);
        break;
    }
    
    switch (current_midi_echo) {
    case FA_MIDI_NO_OUTPUT:
        send_osc(message, user_data, "/current/midi/echo", "N");
        break;
    case FA_MIDI_TO_AUDIO:
        send_osc(message, user_data, "/current/midi/echo", "T");
        break;
    case FA_MIDI_TO_DEVICE:
        assert(current_midi_echo_device && "current_midi_echo_device is NULL in current_devices_handler");
        char *host_name = fa_dunstring(fa_midi_host_name(current_midi_echo_device));
        char *name = fa_dunstring(fa_midi_name(current_midi_echo_device));
        send_osc(message, user_data, "/current/midi/echo", "ssii", host_name, name,
            fa_midi_has_input(current_midi_echo_device) ? 1 : 0,
            fa_midi_has_output(current_midi_echo_device) ? 1 : 0);
        fa_free(host_name);
        fa_free(name);
        break;
    }
}

int current_devices_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    send_current_devices(message, user_data);
    return 0;
}

void send_stream_info(lo_message message, void *user_data)
{
    if (current_audio_stream) {
        fa_map_t info = fa_audio_stream_get_info(current_audio_stream);
        fa_list_t keys = fa_map_get_keys(info);
        fa_for_each (key, keys) {
            fa_ptr_t value = fa_map_get(key, info);
            send_fa_value_osc(message, user_data, "/stream/info", fa_unstring(key), value);
        }
        fa_destroy(keys);
        fa_destroy(info);
    } else {
        fa_slog_warning("send_stream_info: no audio stream!");
    }
}

int stream_info_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    send_stream_info(message, user_data);
    return 0;
}

/***************************
*   /send/midi
*/

int simple_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    fa_midi_message_t midi_message = fa_midi_message_create_simple(argv[0]->i, argv[1]->i, (argc > 2) ? argv[2]->i : 0);
    fa_action_t action = fa_action_send(synth_name, midi_message);
    schedule_now(action, current_midi_playback_stream);
    return 0;
}

/***************************
*   /send/note
*/

int simple_note_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    double f0 = argv[0]->f;
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
    // schedule_relative(fa_now(), action, current_midi_playback_stream);
    
    int pitch = f0; // truncate
    uint8_t cents = ch == 9 ? 0 : round((double)(f0 - pitch) * (double)100.0);
    
    // printf("%f => %d %d (%d)\n", f0, pitch, cents, vel);
    
    fa_action_t noteOn  = fa_action_send(synth_name, fa_midi_message_create_extended(0x90 + ch, pitch, vel, cents));
    fa_action_t noteOff = fa_action_send(synth_name, fa_midi_message_create_extended(0x90 + ch, pitch, 0, cents));
    schedule_now(noteOn, current_midi_playback_stream);
    schedule_relative(fa_milliseconds(argv[2]->i), noteOff, current_midi_playback_stream);
    return 0;
}

/***************************
*   /receive/midi
*/

int receive_midi_handler(const char *path, const char *types, lo_arg ** argv, int argc, void *data, void *user_data)
{
    if (!current_clock) return 0;
    fa_midi_message_t midi_message = fa_midi_message_create_simple(argv[0]->i, argv[1]->i, (argc > 2) ? argv[2]->i : 0);
    bool echo_to_playback = (bool)user_data;
    handle_incoming_midi(fa_clock_time(current_clock), midi_message, echo_to_playback);
    return 0;
}


/***************************
*   /audio-file/list
*/

int list_audio_files_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int id = argc > 0 ? argv[0]->i : -1;
    
    fa_with_lock(audio_files_mutex) {
        fa_list_t keys = fa_map_get_keys(audio_files);
        size_t count = 0;
        fa_for_each (key, keys) {
            fa_ptr_t buffer = fa_map_get(key, audio_files);
            if (buffer) {
                int audio_id = fa_peek_integer(key);
                fa_ptr_t sample_rate = fa_get_meta(buffer, fa_string("sample-rate"));
                fa_ptr_t channels    = fa_get_meta(buffer, fa_string("channels"));
                if (sample_rate && channels) {
                    uint32_t sr = safe_peek_i32(sample_rate);
                    uint32_t ch = safe_peek_i32(channels);
                    size_t frames = fa_buffer_size(buffer) / (sizeof(double) * ch);
                    send_osc(message, user_data, "/audio-file", "iiii", audio_id, frames, sr, ch);
                    count++;
                } else {
                    fa_slog_error("sample_rate or channels missing for audio id ", key);
                }
            } else {
                fa_slog_warning("No buffer for audio id ", key);
            }
        }
        fa_destroy(keys);
        send_osc(message, user_data, "/audio-file/list", "ii", id, count);
    }
    return 0;
}

int load_audio_file_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_string_t file_path = fa_string_from_utf8(&argv[1]->s);
    size_t max_size = argc > 2 ? argv[2]->i : kMaxInMemoryFile;
    fa_ptr_t buffer = fa_buffer_read_audio_max_size(file_path, max_size, false);
    bool mp3 = false;
#ifdef FA_MP3_IMPORT
    if (buffer && fa_check(buffer)) {
        fa_destroy(buffer);
        buffer = fa_buffer_read_mp3_max_size(file_path, max_size, false);
        mp3 = true;
    }
#endif
    
    // The audio file fit into max_size bytes of memory, so we now have the
    // audio file in a memory buffer (or possibly an error value)
    if (buffer) {
        if (fa_check(buffer)) {
            fa_error_log(NULL, (fa_error_t) buffer); // this destroys buffer (the error)
            fa_destroy(file_path);
            send_osc(message, user_data, "/audio-file/load", "iFs", id, "couldNotReadFile");
            return 0;
        }
        fa_slog_info("File was loaded into memory");
    }
    
    // fa_buffer_read_audio_max_size returned NULL, which means that the audio file
    // was too big. Instead, we create a file_buffer.
    else {
        fa_slog_info("File was too big to load into memory, so we use a file_buffer instead");
    
        if (mp3) {
#ifdef FA_MP3_IMPORT
            buffer = fa_file_buffer_read_mp3(file_path, kFileBufferSize, float_sample_type); // 2 MB buffer
#endif
        } else {
            buffer = fa_file_buffer_read_audio(file_path, kFileBufferSize, float_sample_type); // 2 MB buffer
        }
        if (fa_check(buffer)) {
            fa_error_log(NULL, (fa_error_t) buffer); // this destroys buffer (the error)
            fa_destroy(file_path);
            send_osc(message, user_data, "/audio-file/load", "iFs", id, "couldNotReadFile");
            return 0;
        }
    }
    
    fa_set_meta(buffer, fa_string("file_path"), file_path); // leave ownership of file_path
    fa_with_lock(audio_files_mutex) {
        audio_files = fa_map_dset(wrap_oid(id), buffer, audio_files);
    }
    fa_ptr_t frames      = fa_get_meta(buffer, fa_string("frames"));
    fa_ptr_t sample_rate = fa_get_meta(buffer, fa_string("sample-rate"));
    fa_ptr_t channels    = fa_get_meta(buffer, fa_string("channels"));
    fa_ptr_t cropped     = fa_get_meta(buffer, fa_string("cropped"));
    if (frames && sample_rate && channels) {
        uint32_t fr = fa_peek_integer(frames); // in 96kHz, this is still over 12h, so we should be safe with 32 bits
        uint32_t sr = safe_peek_i32(sample_rate);
        uint32_t ch = safe_peek_i32(channels);
        if (cropped && fa_peek_bool(cropped)) {
            send_osc(message, user_data, "/audio-file/load", "iTiiiT", id, fr, sr, ch);
        } else {
            send_osc(message, user_data, "/audio-file/load", "iTiiiF", id, fr, sr, ch);
        }
    } else {
        fa_fail(fa_string("frames, sample-rate or channels not set"));
        send_osc(message, user_data, "/audio-file/load", "iFs", id, "missingMetaData");
    }

    return 0;
    
}

int load_raw_audio_file_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id      = argv[0]->i;
    fa_string_t file_path = fa_string_from_utf8(&argv[1]->s);
    int sr        = argv[2]->i;
    int ch        = argv[3]->i;
    float latency = argc > 4 ? argv[4]->f : 0;
    
    if (ch < 1 || ch > 2 || sr < 44100 || sr > 192000) {
        fa_destroy(file_path);
        fa_fail(fa_string("Bad sample rate and/or channels in load_raw_audio_file_handler"));
        send_osc(message, user_data, "/audio-file/load", "iFs", id, "badSampleRateOrChannels");
        return 0;
    }
    
    
    fa_ptr_t buffer = fa_buffer_read_raw_max_size(file_path, kMaxInMemoryFile);
    if (buffer) {
        if (fa_check(buffer)) {
            fa_error_log(NULL, (fa_error_t) buffer); // this destroys buffer (the error)
            fa_destroy(file_path);
            send_osc(message, user_data, "/audio-file/load", "iFs", id, "couldNotReadFile");
            return 0;
        }
        if (fa_buffer_size(buffer) == 0) {
            fa_destroy(file_path);
            fa_destroy(buffer);
            send_osc(message, user_data, "/audio-file/load", "iFs", id, "emptyFile");
            return 0;
        }
        fa_slog_info("Read raw file into memory ", buffer, file_path, fa_i32(fa_buffer_size(buffer)));
    } else {
        buffer = fa_file_buffer_create(file_path, kFileBufferSize);
        if (fa_check(buffer)) {
            fa_error_log(NULL, (fa_error_t) buffer); // this destroys buffer (the error)
            fa_destroy(file_path);
            send_osc(message, user_data, "/audio-file/load", "iFs", id, "couldNotReadFile");
            return 0;
        }
        fa_slog_info("Opened file_buffer for raw file ", buffer, fa_i32(fa_file_buffer_file_size(buffer)));
    }
    
    size_t frames = fa_buffer_size(buffer) / (sizeof(double) * ch);
    fa_set_meta(buffer, fa_string("file_path"), file_path);
    fa_set_meta(buffer, fa_string("latency"), fa_from_float(latency));
    fa_set_meta(buffer, fa_string("sample-rate"), fa_from_int32(sr));
    fa_set_meta(buffer, fa_string("channels"), fa_from_int32(ch));
    fa_set_meta(buffer, fa_string("frames"), fa_from_int64(frames));
    fa_set_meta(buffer, fa_string("sample-size"), fa_i8(fa_sample_type_size(double_sample_type)));
    fa_set_meta(buffer, fa_string("sample-type"), fa_i8(double_sample_type));
    
    fa_with_lock(audio_files_mutex) {
        audio_files = fa_map_dset(wrap_oid(id), buffer, audio_files);
    }
    
    fa_slog_info("Raw file info: ", fa_get_meta(buffer, fa_string("sample-rate")), fa_get_meta(buffer, fa_string("channels")));
    fa_slog_info("(continued)  : ", fa_get_meta(buffer, fa_string("frames")), fa_get_meta(buffer, fa_string("sample-size")));

    send_osc(message, user_data, "/audio-file/load", "iTiiiF", id, (uint32_t)frames, sr, ch);
    
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

int audio_file_meta_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id         = argv[0]->i;
    oid_t audio_id   = argv[1]->i;
    char *key        = &argv[2]->s;
    char *value      = (argc >= 4 && types[3] == 's') ? &argv[3]->s : NULL;

    bool ok = 0;
    fa_with_lock(audio_files_mutex) {
        fa_ptr_t buffer = fa_map_dget(wrap_oid(audio_id), audio_files);
        if (buffer) {
            fa_string_t the_key = fa_dappend(fa_string("id3:"), fa_string_from_utf8(key));
            fa_string_t the_value = value ? fa_string_from_utf8(value) : NULL;
            fa_set_meta(buffer, the_key, the_value);
            ok = 1;
        }
    }
    if (ok) {
        send_osc(message, user_data, path, "iT", id);
    } else {
        send_osc(message, user_data, path, "iF", id);
    }
    return 0;
}

int save_audio_file_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id                = argv[0]->i;
    oid_t audio_id          = argv[1]->i;
    // fa_string_t target_path = fa_string_from_utf8(&argv[1]->s);
    // char *path     = &argv[2]->s;

    if (verbose) fa_inform(fa_dappend(fa_string("save_audio_file_handler "), fa_string_from_utf8(path)));

    fa_ptr_t buffer = NULL;
    // bool new_buffer = false;
    fa_io_source_t source = NULL;
    size_t channels = 0;
    size_t sample_rate = 0;
    
    fa_with_lock(audio_files_mutex) {
        buffer = fa_map_dget(wrap_oid(audio_id), audio_files);
        switch (fa_dynamic_get_type(buffer)) {
        case buffer_type_repr: {
            if (verbose) fa_slog_info("Saving contents of memory buffer to audio file", buffer);
            fa_take_reference(buffer);
            channels = safe_peek_i32(fa_get_meta(buffer, fa_string("channels")));
            sample_rate = safe_peek_i32(fa_get_meta(buffer, fa_string("sample-rate")));
            source = fa_io_from_buffer(buffer);
            break;
        }
        case file_buffer_type_repr: {
            if (verbose) fa_slog_info("Saving contents of file buffer ", buffer);
            fa_take_reference(buffer);
            channels = safe_peek_i32(fa_get_meta(buffer, fa_string("channels")));
            sample_rate = safe_peek_i32(fa_get_meta(buffer, fa_string("sample-rate")));
            if (!channels || !sample_rate) {
                fa_fail(fa_string("Unknown number of channels and/or sample rate!"));
                break;
            }
            fa_ptr_t audio_format = fa_get_meta(buffer, fa_string("audio-format"));
            fa_string_t file_path = fa_get_meta(buffer, fa_string("file_path"));
            if (file_path) {
                file_path = fa_copy(file_path);
                fa_slog_info(" file path is ", file_path);
            } else {
                fa_fail(fa_format_integral("Audio file %zu has no path", id));
                break;
            }
            
            if (audio_format && fa_dequal(fa_copy(audio_format), fa_string("mp3"))) {
#ifdef FA_MP3_IMPORT
                source = fa_io_read_mp3_file(file_path); // Audio file
#endif
            } else if (audio_format) {
                source = fa_io_read_audio_file(file_path); // Audio file
            } else {
                source = fa_io_read_file(file_path); // Raw file
            }
            break;
        }
        default: {
            fa_fail(fa_format_integral("There is no audio file with ID %zu", audio_id));
            send_osc(message, user_data, path, "iF", id);
            buffer = NULL;
            break;
        }
        } // switch
    }
    if (!source) {
        fa_fail(fa_string("No source! Unknown file format?"));
        send_osc(message, user_data, path, "iF", id);
        if (buffer) fa_release_reference(buffer);
        return 0;
    }
    
    fa_string_t target_path = fa_string_from_utf8(&argv[2]->s);
    fa_io_sink_t sink = NULL;
    if (strcmp(path, "/audio-file/save/aiff") == 0) {
        sink = fa_io_write_audio_file(target_path, channels, sample_rate, SF_FORMAT_AIFF | SF_FORMAT_PCM_16);
    } else if (strcmp(path, "/audio-file/save/wav") == 0) {
        sink = fa_io_write_audio_file(target_path, channels, sample_rate, SF_FORMAT_WAV | SF_FORMAT_PCM_16);
    } else if (strcmp(path, "/audio-file/save/ogg") == 0) {
        float ogg_quality = (argc >= 4) ? argv[3]->f : default_ogg_quality;
        if (verbose) fa_inform(fa_format("Exporting with ogg quality %f\n", ogg_quality));
        source = fa_io_apply(source, fa_io_create_ogg_encoder(sample_rate, channels, ogg_quality));
        sink = fa_io_write_file(target_path);
    } else if (strcmp(path, "/audio-file/save/mp3") == 0) {
        int mp3_bitrate = (argc >= 4) ? argv[3]->i : default_mp3_bitrate;
        if (verbose) fa_inform(fa_format("Exporting with mp3 bitrate %d\n", mp3_bitrate));
        fa_map_t id3 = fa_meta_map(buffer); // TODO: check that map is not changed in other process!
        // id3 = fa_map_dset(fa_string("TIT2"), fa_string("En titel med åäö i"), id3);
        // id3 = fa_map_dset(fa_string("artist"), fa_string("En artiståäöaerr"), id3);
        sink = fa_io_write_mp3_file(target_path, channels, sample_rate, mp3_bitrate, id3);
    } else if (strcmp(path, "/audio-file/save/raw") == 0) {
        sink = fa_io_write_file(target_path);
    } else {
        assert(false && "Unknown target format");
    }
    if (fa_check(sink)) {
        send_osc(message, user_data, path, "iFs", id, fa_string_to_utf8(fa_error_message((fa_error_t)sink)));
    } else {
        fa_io_run(source, sink);
        send_osc(message, user_data, path, "iT", id);
    }

    // Cleanup
    fa_destroy(source);
    fa_destroy(sink);
    fa_destroy(target_path);
    fa_release_reference(buffer);

    return 0;
}


int audio_file_curve_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
    fa_with_lock(audio_files_mutex) {
        fa_buffer_t buffer = fa_map_dget(wrap_oid(id), audio_files);
        if (buffer) {
            //lo_blob blob = audio_curve(buffer);
            
            fa_buffer_t curve = audio_curve(buffer);
            //fa_slog_info("audio_file_curve_handler  curve: ", curve);
            if (!curve) {
                fa_fail(fa_format_integral("Could not generate audio curve for audio file %zu", id));
                send_osc(message, user_data, "/audio-file/curve", "iF", id);
                continue; // cannot return directly, that won't release the lock
            }
            size_t curve_size = fa_buffer_size(curve);
            //printf("audio_file_curve_handler curve_size: %zu", curve_size);
            if (curve_size <= 16384) {
                lo_blob blob = lo_blob_from_buffer(curve);
                //printf("audio_file_curve_handler curve_size <= 16384, blob: %p", blob);
                send_osc(message, user_data, "/audio-file/curve", "ib", id, blob);
                lo_blob_free(blob);
            } else {
                fa_list_t segments = fa_buffer_split(curve, 16384, false);
                //fa_slog_info("audio_file_curve_handler curve_size > 16384, segments: segments", segments);
                size_t offset = 0;
                fa_for_each (segment, segments) {
                    //fa_slog_info("segments: ", segment);
                    lo_blob blob = lo_blob_from_buffer(segment);
                    //printf(" => blob: %p", blob);
                    send_osc(message, user_data, "/audio-file/curve", "ibii", id, blob, offset, curve_size);
                    lo_blob_free(blob);
                    offset += fa_buffer_size(segment);
                }
            }
            fa_destroy(curve);
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

int audio_file_upload_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id         =  argv[0]->i;
    oid_t audio_id   =  argv[1]->i;
    char* url        = &argv[2]->s;
    char* cookie     = &argv[3]->s;
    char* dump_file  = argc >= 7 ? &argv[6]->s : NULL;
    fa_ptr_t buffer  = NULL;
    size_t sample_rate;
    size_t channels;
    
    bool new_buffer = false;
    fa_io_source_t raw_source = NULL;
    
    fa_with_lock(audio_files_mutex) {
        buffer = fa_map_dget(wrap_oid(audio_id), audio_files);
        switch (fa_dynamic_get_type(buffer)) {
        case buffer_type_repr: {
            fa_take_reference(buffer);
            fa_ptr_t sr = fa_get_meta(buffer, fa_string("sample-rate"));
            fa_ptr_t ch = fa_get_meta(buffer, fa_string("channels"));
            channels = fa_peek_integer(ch);
            sample_rate = fa_peek_number(sr);
            
            // Use only part of buffer
            // TODO: don't copy, implement a filter in fa_io_from_buffer instead, to reduce memory footprint
            if (argc >= 5) {
                double from_ms = argv[4]->f;
                double to_ms   = argv[5]->f;
                if (from_ms < 0) from_ms = 0;
                //if (to_ms < 0) to_ms = 0;
                if (from_ms > 0 || to_ms >= 0) {
                    size_t from_byte = sizeof(double) * (int)round((double)sample_rate * (from_ms / 1000.0) * (double)channels);
                    size_t to_byte;
                    if (to_ms < 0) to_byte = fa_buffer_size(buffer);
                    else to_byte = sizeof(double) * (int)round((double)sample_rate * (to_ms / 1000.0) * (double)channels);
                    if (to_byte > fa_buffer_size(buffer)) to_byte = fa_buffer_size(buffer);
                    size_t size = to_byte - from_byte;
                    if (verbose) fa_inform(fa_format_integral("size: %zu", size));
                    if (size > 0) {
                        uint8_t *src = fa_buffer_unsafe_address(buffer);
                        uint8_t *dst = fa_malloc(size);
                        memcpy(dst, src + from_byte, size);
                        fa_release_reference(buffer);
                        buffer = fa_buffer_dwrap(dst, size);
                        new_buffer = true;
                    }
                }
            }
            raw_source = fa_io_from_buffer(buffer);
            break;
        }
        case file_buffer_type_repr: {
            fa_take_reference(buffer);
            fa_ptr_t sr = fa_get_meta(buffer, fa_string("sample-rate"));
            fa_ptr_t ch = fa_get_meta(buffer, fa_string("channels"));
            channels = fa_peek_integer(ch);
            sample_rate = fa_peek_number(sr);
            
            fa_file_buffer_t file_buffer = buffer;
            fa_string_t path = fa_copy(fa_file_buffer_path(file_buffer));
            fa_ptr_t audio_format = fa_get_meta(file_buffer, fa_string("audio-format"));
            
            if (argc >= 5) {
                double from_ms = argv[4]->f;
                double to_ms   = argv[5]->f;
                if (from_ms < 0) from_ms = 0;
                if (audio_format) {
                    // File is an audio file
                    size_t from_frame = (int)round((double)sample_rate * (from_ms / 1000.0));
                    size_t to_frame = (int)round((double)sample_rate * (to_ms / 1000.0));
                    if (fa_dequal(fa_copy(audio_format), fa_string("mp3"))) {
#ifdef FA_MP3_IMPORT
                        raw_source = fa_io_read_mp3_file_between(path, fa_i32(from_frame), (to_ms >= 0) ? fa_i32(to_frame) : NULL);
#endif
                    } else {
                        raw_source = fa_io_read_audio_file_between(path, fa_i32(from_frame), (to_ms >= 0) ? fa_i32(to_frame) : NULL);
                    }
                } else {
                    // File is a raw file
                    uint8_t frame_size = channels * fa_sample_type_size(double_sample_type); // Assume doubles
                    size_t from_byte = (int)round((double)sample_rate * (double)frame_size * (from_ms / 1000.0));
                    size_t to_byte = (int)round((double)sample_rate * (double)frame_size * (to_ms / 1000.0));
                    raw_source = fa_io_read_file_between(path, fa_i32(from_byte), (to_ms >= 0) ? fa_i32(to_byte) : NULL);
                }
            } else {
                if (audio_format && fa_dequal(fa_copy(audio_format), fa_string("mp3"))) {
#ifdef FA_MP3_IMPORT
                    raw_source = fa_io_read_mp3_file(path); // Audio file
#endif
                } else if (audio_format) {
                    raw_source = fa_io_read_audio_file(path); // Audio file
                } else {
                    raw_source = fa_io_read_file(path); // Raw file
                }
            }
            
            break;
        }
        default: {
            fa_fail(fa_format_integral("There is no audio file with ID %zu", audio_id));
            send_osc(message, user_data, "/audio-file/upload", "iF", id);
            buffer = NULL;
            break;
        }
        } // switch
    }
    if (!raw_source) {
        return 0;
    }
    
    send_osc(message, user_data, "/audio-file/upload/preparing", "i", id);
    
    fa_io_source_t source = fa_io_apply(raw_source, fa_io_create_ogg_encoder(sample_rate, channels, default_ogg_quality));
    fa_buffer_t ogg_buffer = fa_io_pull_to_buffer(source);
    size_t ogg_size = fa_buffer_size(ogg_buffer);
    fa_destroy(source);
    if (new_buffer) {
        fa_destroy(buffer);
    } else {
        fa_release_reference(buffer);
    }
    
    // fa_slog_info("ogg buffer: ", ogg_buffer);
    // printf("Uploading %zu bytes...\n", fa_buffer_size(ogg_buffer));
    
    if (dump_file && dump_file[0]) {
        fa_slog_info("ogg buffer: ", ogg_buffer);
        fa_inform(fa_format_integral("ogg buffer size: %zu", fa_buffer_size(ogg_buffer)));
        fa_inform(fa_dappend(fa_string("Dumping ogg to "), fa_string_from_utf8(dump_file)));
        fa_buffer_write_raw(fa_string_from_utf8(dump_file), ogg_buffer);
    }

    send_osc(message, user_data, "/audio-file/upload/started", "ii", id, ogg_size);
    fa_thread_t thread = upload_buffer_async(id, ogg_buffer, url, cookie);
    fa_thread_detach(thread);
    return 0;
}


/***************************
*   /send/main-volume channel volume (0-16383)
*/

int main_volume_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    int vol = argv[1]->i;
    schedule_now(main_volume_action(ch, vol), current_midi_playback_stream);
    return 0;
}

/***************************
*   /send/pan channel pan
*/

int pan_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    float pan = argv[1]->f;
    schedule_now(pan_action(ch, pan), current_midi_playback_stream);
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
    schedule_now(action, current_midi_playback_stream);
    return 0;
}

/***************************
*   /send/pitch-wheel
*/

int pitch_wheel_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    float pitch = argv[1]->f;
    schedule_now(pitch_wheel_action(ch, pitch), current_midi_playback_stream);
    return 0;
}

/***************************
*   /send/sustain-wheel
*/

int sustain_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    int ch = argv[0]->i;
    bool sustain = argv[1]->i > 0;
    schedule_now(sustain_action(ch, sustain), current_midi_playback_stream);
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
    
    fa_action_t action = fa_action_simultaneous(list(
        main_volume_action(ch, main_volume),
        pan_action(ch, pan),
        bank_select_action(ch, bank),
        program_change_action(ch, program),
        pitch_wheel_action(ch, pitch_wheel),
        sustain_action(ch, sustain)));

    schedule_now(action, current_midi_playback_stream);
    return 0;
}


int stats_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    fa_log_region_count("Stats:");
    fa_list_log_count();
    fa_time_log_count();
    fa_pair_log_count();
    fa_pair_left_log_count();
    fa_string_log_count();
    fa_func_ref_log_count();
    fa_action_log_count();
    fa_map_log_count();
    fa_inform(fa_string_format_integral("recording_state: %d", recording_state));
    fa_slog_info("in_bundle: ", fa_i8(in_bundle));
    return 0;
}


int volume_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    uint32_t channel = (uint32_t) user_data;
    float value = argv[0]->f;
    // Save value for later...
    switch (channel) {
        case kSynthLeft: synth_volume = value; break;
        case kAudioLeft: audio_volume = value; break;
        case kMonitorLeft: monitor_volume = value; break;
        default: assert(false && "Unknown channel in volume_handler");
    }
    // ... but also send it now, if there is an audio stream
    if (current_audio_stream) {
        fa_list_t actions = list(pair(fa_action_set(channel + 0, value), fa_now()),
                                 pair(fa_action_set(channel + 1, value), fa_now()));
        schedule_now(fa_action_many(actions), current_audio_stream);
    }
    return 0;
}

int level_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    bool start = (bool) user_data;
    if (start) {
        start_level_echo();
    } else {
        stop_level_echo();
    }
    return 0;
}

fa_ptr_t _recording_started(fa_ptr_t context, fa_time_t time, fa_time_t now)
{
    fa_with_lock(recording_state_mutex) {
        recording_state = RECORDING_RUNNING;
    }
    if (time) {
        send_osc_async("/recording/started", "it", peek_oid(context), timetag_from_time(time));
    } else {
        send_osc_async("/recording/started", "iN", peek_oid(context));
    }
    return NULL;
}

// fa_ptr_t _recording_stopped(fa_ptr_t context, fa_time_t time, fa_time_t now)
// {
//     oid_t id = peek_oid(context);
//     if (remove_recording_semaphore(id)) {
//         send_osc_async("/recording/stopped", "iT", peek_oid(context)); //, timetag_from_time(now));
//         recording_state = NOT_RECORDING;
//     }
//     return NULL;
// }

fa_ptr_t _stop_recording(fa_ptr_t context) {
    if (recording_state != NOT_RECORDING) {
        fa_with_lock(recording_state_mutex) {
            recording_state = RECORDING_STOPPING;
        }
    }
    fa_slog_info("_stop_recording");
    //_recording_stopped(context, NULL, NULL); // TODO
    
    oid_t id = peek_oid(context);
    if (remove_recording_semaphore(id)) {
        fa_slog_info("_stop_recording: successfully removed semaphore");
        // Don't send osc here, the recording may not be entirely finished
        //send_osc_async("/recording/stopped", "is", peek_oid(context), "auto"); //, timetag_from_time(now));
    } else {
        fa_slog_warning("_stop_recording: could not remove semaphore");
    }
    return NULL;
}

int start_recording_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    // NOTE: we support only one recording at a time
    oid_t id          = argv[0]->i;
    char* filename    = &argv[1]->s;
    char* url         = &argv[2]->s;
    char* cookies     = &argv[3]->s;
    double rel_time   = argc > 4 ? argv[4]->f : 0;
    double max_length = argc > 5 ? argv[5]->f : 0;
    
    check_id(id, message, user_data);
    
    fa_slog_info("start_recording_handler");
    
    if (!url || (*url == 0)) {
        fa_slog_info("Recording without upload");
        url = NULL; // Empty URL is same as no URL
    }
  
  	// No audio stream
  	if (!current_audio_stream || current_sample_rate == 0) {
        fa_slog_warning("Cannot record: no audio stream running!");
        send_osc(message, user_data, "/recording/start", "iFs", id, "no-audio-stream");
        return 0;
  	}
    
    // No input device
    if (!current_audio_input_device) {
        fa_slog_warning("Cannot record: no audio input device!");
        send_osc(message, user_data, "/recording/start", "iFs", id, "no-audio-input");
        return 0;
    }
    
    // Check that either filename or URL is provided (or both)
    if (*filename == 0 && !url) {
        fa_fail(fa_string("Cannot record: URL or filename must be provided!"));
        send_osc(message, user_data, "/recording/start", "iFs", id, "no-url-or-filename");
        return 0;
    }
    
    // If filename is provided, test that the path is writeable
    if (*filename) {
        FILE *fp = fa_fopen(filename, "ab");
        if (fp) {
            fclose(fp);
        } else {
            fa_fail(fa_dappend(fa_string("Cannot record: path is not writeable: "), fa_string_from_utf8(filename)));
            fa_inform(fa_string_format_integral("  Error code: %d", errno));
            send_osc(message, user_data, "/recording/start", "iFs", id, "path-not-writeable");
            return 0;
        }
    }
    
    // Already recording
    {
        bool cancel = false;
        fa_with_lock(recording_state_mutex) {
            if (recording_state == NOT_RECORDING) {
                recording_state = RECORDING_INITIALIZING;
            } else {
                fa_slog_warning("Cannot record: already recording, state: ", fa_from_int8(recording_state));
                send_osc(message, user_data, "/recording/start", "iFs", id, "already-recording");
                cancel = true; // cannot return here, lock wouldn't be released
            }
        }
        if (cancel) return 0;
    }
    
    add_recording_semaphore(id);
    
    // fa_slog_info("recording_semaphores: ", recording_semaphores);
    
    fa_atomic_ring_buffer_reset(recording_ring_buffer);
    
    recording_thread = create_recording_thread(id, recording_ring_buffer, filename, url, cookies, current_sample_rate, 1);
    
    fa_action_t action = fa_action_send_retain(record_left_name, recording_ring_buffer);
    //if (rel_time == 0) {
    //    schedule_now(action, current_audio_stream);
    //    _recording_started(wrap_oid(id), NULL, NULL); // TODO: time
    //} else {
    action = fa_action_if(check_recording_semaphore, wrap_oid(id),
                fa_action_many(list(pair(action, fa_now()),
                                    pair(fa_action_do_with_time(_recording_started, wrap_oid(id)), fa_now()))));
    schedule_relative(fa_time_from_double(rel_time), action, current_audio_stream);
    //}
    
    if (max_length < 0) {
        fa_warn(fa_string("start_recording_handler: negative max length! Disabling max length (setting to 0)"));
        max_length = 0;
    }
    
    if (max_length > 0) {
        fa_action_t action = fa_action_if(check_recording_semaphore, wrap_oid(id),
            fa_action_many(list(
                pair(fa_action_send(record_left_name, NULL), fa_now()),
                pair(fa_action_send(record_right_name, NULL), fa_now()),
                pair(fa_action_do(_stop_recording, wrap_oid(id)), fa_now())))); // has to be after the sends! May still be too early
        schedule_relative(fa_time_from_double(rel_time + max_length), action, current_audio_stream);
    }
    
    send_osc(message, user_data, "/recording/start", "iT", id);
    
    return 0;
}

int stop_recording_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    oid_t id = argv[0]->i;
	
	if (verbose) fa_inform(fa_format_integral("stop_recording_handler called with id %hu", id));

    fa_with_lock(recording_state_mutex) {
        if (recording_state == RECORDING_INITIALIZING) {
            if (remove_recording_semaphore(id)) {
                fa_slog_info("Aborting recording before it actually started");
                send_osc(message, user_data, "/recording/stop", "iT", id);
                recording_state = RECORDING_STOPPING;
                fa_action_t action = fa_action_many(list(
                    pair(fa_action_send(record_left_name, NULL), fa_now()),
                    pair(fa_action_send(record_right_name, NULL), fa_now())));
                schedule_now(action, current_audio_stream);
                send_osc(message, user_data, "/recording/stopped", "is", id, "early");
            } else {
                fa_slog_info("Recording is initializing under another ID");
                send_osc(message, user_data, "/recording/stop", "iFs", id, "not-recording");
            }
            continue;
        }
        if (recording_state == NOT_RECORDING || !check_recording_semaphore(wrap_oid(id), NULL)) {
            fa_slog_warning("Cannot stop recording: not currently recording at id ", wrap_oid(id));
            fa_slog_info("recording_semaphores: ", recording_semaphores);
            fa_inform(fa_format_integral("recording_state: %d", recording_state));
            send_osc(message, user_data, "/recording/stop", "iFs", id, "not-recording");
            remove_recording_semaphore(id); // just in case
            continue; // cannot use return, that wouldn't release the lock
        }
    
        if (remove_recording_semaphore(id)) {
            fa_slog_info("Stopping recording...");
            send_osc(message, user_data, "/recording/stop", "iT", id);
            recording_state = RECORDING_STOPPING;
            fa_action_t action = fa_action_many(list(
                pair(fa_action_send(record_left_name, NULL), fa_now()),
                pair(fa_action_send(record_right_name, NULL), fa_now())));
            schedule_now(action, current_audio_stream);
            //send_osc_async("/recording/stopped", "is", id, "stopped"); // May be too early!!
        } else {
            fa_slog_warning("Could not remove recording semaphore (strange!) ", wrap_oid(id));
            send_osc(message, user_data, "/recording/stop", "iFs", id, "strange-error");
        }
    }
    return 0;
}

int choose_device_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (noaudio) {
        if (verbose) fa_slog_info("Audio disabled, ignoring choose_device");
        return 0;
    }

    switch ((int)user_data) {
    // Audio input
    case CHOOSE_AUDIO_INPUT_NONE:
        if (selected_audio_input_device) fa_deep_destroy_always(selected_audio_input_device);
        selected_audio_input_device = NULL;
        break;
    case CHOOSE_AUDIO_INPUT_DEFAULT:
        if (selected_audio_input_device) fa_deep_destroy_always(selected_audio_input_device);
        selected_audio_input_device = pair(fa_string_empty(), fa_string_empty());
        break;
    case CHOOSE_AUDIO_INPUT_DEVICE:
        if (selected_audio_input_device) fa_deep_destroy_always(selected_audio_input_device);
        selected_audio_input_device = pair(fa_string_from_utf8(&argv[0]->s), fa_string_from_utf8(&argv[1]->s));
        break;
    // Audio output
    case CHOOSE_AUDIO_OUTPUT_NONE:
        if (selected_audio_output_device) fa_deep_destroy_always(selected_audio_output_device);
        selected_audio_output_device = NULL;
        break;
    case CHOOSE_AUDIO_OUTPUT_DEFAULT:
        if (selected_audio_output_device) fa_deep_destroy_always(selected_audio_output_device);
        selected_audio_output_device = pair(fa_string_empty(), fa_string_empty());
        break;
    case CHOOSE_AUDIO_OUTPUT_DEVICE:
        if (selected_audio_output_device) fa_deep_destroy_always(selected_audio_output_device);
        selected_audio_output_device = pair(fa_string_from_utf8(&argv[0]->s), fa_string_from_utf8(&argv[1]->s));
        break;
    // MIDI input
    case CHOOSE_MIDI_INPUT_NONE:
        if (selected_midi_input_devices) fa_deep_destroy_always(selected_midi_input_devices);
        selected_midi_input_devices = fa_list_empty();
        break;
    case CHOOSE_MIDI_INPUT_ALL:
        if (selected_midi_input_devices) fa_deep_destroy_always(selected_midi_input_devices);
        selected_midi_input_devices = NULL;
        break;
    case CHOOSE_MIDI_INPUT_DEVICE:
        if (selected_midi_input_devices) fa_deep_destroy_always(selected_midi_input_devices);
        selected_midi_input_devices = list(pair(fa_string_from_utf8(&argv[0]->s), fa_string_from_utf8(&argv[1]->s)));
        break;
    // MIDI playback
    case CHOOSE_MIDI_PLAYBACK_NONE:
        selected_midi_playback = FA_MIDI_NO_OUTPUT;
        break;
    case CHOOSE_MIDI_PLAYBACK_AUDIO:
        selected_midi_playback = FA_MIDI_TO_AUDIO;
        break;
    case CHOOSE_MIDI_PLAYBACK_DEVICE:
        selected_midi_playback = FA_MIDI_TO_DEVICE;
        if (selected_midi_playback_device) fa_deep_destroy_always(selected_midi_playback_device);
        selected_midi_playback_device = pair(fa_string_from_utf8(&argv[0]->s), fa_string_from_utf8(&argv[1]->s));
        break;
    // MIDI echo type
    case CHOOSE_MIDI_ECHO_NONE:
        selected_midi_echo = FA_ECHO_NO_ECHO;
        break;
    case CHOOSE_MIDI_ECHO_PLAYBACK:
        selected_midi_echo = FA_ECHO_TO_PLAYBACK;
    case CHOOSE_MIDI_ECHO_AUDIO:
        selected_midi_echo = FA_ECHO_TO_AUDIO;
        break;
    case CHOOSE_MIDI_ECHO_DEVICE:
        selected_midi_echo = FA_ECHO_TO_DEVICE;
        if (selected_midi_echo_device) fa_deep_destroy_always(selected_midi_echo_device);
        selected_midi_echo_device = pair(fa_string_from_utf8(&argv[0]->s), fa_string_from_utf8(&argv[1]->s));
        break;
    // MIDI echo channel
    case CHOOSE_MIDI_ECHO_CHANNEL:
        {
            int ch = -1;
            if (argc > 0) {
                ch = argv[0]->i;
                if (ch < -1 || ch >= 16) {
                    fa_fail(fa_string_format_integral("Bad channel: %d", ch));
                    break;
                }
            }
            midi_echo_channel = ch;
            break;
        }
    default:
        assert(false && "Never reached");
    }
    
    return 0;
}

int restart_streams_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (noaudio) {
        if (verbose) fa_slog_info("Audio disabled, ignoring restart_streams");
        return 0;
    }
    stop_streams();
    remove_all_playback_semaphores();
    remove_all_recording_semaphores();
    recording_state = NOT_RECORDING; // no lock needed here (?)
    resolve_devices();
    start_streams();
    send_all_devices(-1, message, user_data);
    send_current_devices(message, user_data);
    send_stream_info(message, user_data);
    if (argc > 0) {
        send_osc(message, user_data, "/restart/streams", "i", argv[0]->i);
    }
    // printf("In restart_streams_handler, current_sample_rate: %f\n", current_sample_rate);
    return 0;
}

int restart_sessions_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (noaudio) {
        if (verbose) fa_slog_info("Audio disabled, ignoring restart_sessions");
        return 0;
    }
    stop_streams();
    remove_all_playback_semaphores();
    remove_all_recording_semaphores();
    recording_state = NOT_RECORDING; // no lock needed here (?)
    stop_sessions();
    start_sessions();
    resolve_devices();
    start_streams();
    send_all_devices(-1, message, user_data);
    send_current_devices(message, user_data);
    send_stream_info(message, user_data);
    if (argc > 0) {
        send_osc(message, user_data, "/restart/sessions", "i", argv[0]->i);
    }
    // printf("In restart_sessions_handler, current_sample_rate: %f\n", current_sample_rate);
    return 0;
}

int stop_streams_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (noaudio) {
        if (verbose) fa_slog_info("Audio disabled, ignoring stop_streams");
        return 0;
    }
    fa_slog_info("Stopping streams");
    if (recording_state != NOT_RECORDING) {
        fa_slog_warning("Recording in progress, callbacks will probably not be called correctly");
        recording_state = NOT_RECORDING;
    }
    stop_streams();
    if (argc > 0) {
        send_osc(message, user_data, "/stop/streams", "i", argv[0]->i);
    }
    return 0;
}


int wasapi_hack_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
#if _WIN32
    bool enabled = (types[0] == 'T');
    avoid_wasapi_exclusive_bidirectional = enabled;
    fa_inform(fa_dappend(fa_string("WASAPI hack is now "), fa_string_from_utf8(enabled ? "on" : "off")));
#endif
    return 0;
}


int stream_direction_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (noaudio) {
        if (verbose) fa_slog_info("Audio disabled, ignoring stream_direction");
        return 0;
    }
    stream_type_t direction;
    if (strcmp(&argv[0]->s, "output-only") == 0) {
        direction = OUTPUT_ONLY;
    } else if (strcmp(&argv[0]->s, "input-only") == 0) {
        direction = INPUT_ONLY;
    } else if (strcmp(&argv[0]->s, "bidirectional") == 0) {
        direction = BIDIRECTIONAL;
    } else {
        fa_fail(fa_dappend(fa_string("Unknown stream direction: "), fa_string_from_utf8(&argv[0]->s)));
        return 0;
    }
    set_stream_direction(direction);
    return 0;
}

int proxy_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    bool https = (bool)user_data;
    char *proxy_var = https ? https_proxy : http_proxy;
    char *login_var = https ? https_proxy_userpwd : http_proxy_userpwd;
    if (proxy_var) free(proxy_var);
    if (types[0] == 's') {
        proxy_var = strdup(&argv[0]->s);
    } else {
        proxy_var = NULL;
    }
    if (login_var) free(login_var);
    if (argc > 1) {
        char *username = curl_easy_escape(NULL, &argv[1]->s, 0);
        char *password = curl_easy_escape(NULL, &argv[2]->s, 0);
        size_t usersize = strlen(username);
        size_t passsize = strlen(password);
        login_var = malloc(usersize + passsize + 2);
        memcpy(login_var, username, usersize);
        login_var[usersize] = ':';
        memcpy(login_var + usersize + 1, password, passsize);
        login_var[usersize + passsize + 1] = 0;
    } else {
        login_var = NULL;
    }
    return 0;
}

int sleep_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
{
    if (verbose) fa_inform(fa_format_integral("sleep_handler  going to sleep for %d ms", argv[0]->i));
    fa_thread_sleep(argv[0]->i);
    if (verbose) fa_slog_info("sleep_handler  woke up");
    send_osc(message, user_data, "/sleep", "");
    return 0;
}

// int work_handler(const char *path, const char *types, lo_arg ** argv, int argc, lo_message message, void *user_data)
// {
//     if (verbose) fa_inform(fa_format_integral("work_handler  going to work for %d ms", argv[0]->i));
//     struct timeval t0, t1;
//     gettimeofday(&t0);
//     while (1) {
//         gettimeofday(&t1);
//         if ()
//     }
//     fa_thread_sleep(argv[0]->i);
    
//     if (verbose) fa_slog_info("work_handler  back from work");
//     send_osc(message, user_data, "/work");
//     return 0;
// }
