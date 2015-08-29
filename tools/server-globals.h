#include <fa/fa.h>

#ifndef __SERVER_GLOBALS
#define __SERVER_GLOBALS

oid_t last_used_id = 0;

int in_bundle = 0;
fa_list_t bundle_actions = NULL;
fa_time_t bundle_time = NULL;
fa_ptr_t bundle_stream = NULL;

fa_audio_session_t current_audio_session = NULL;
fa_midi_session_t current_midi_session = NULL;
int session_count = 0;

fa_string_t synth_name = NULL;
fa_string_t audio_name = NULL;
fa_string_t record_left_name  = NULL;
fa_string_t record_right_name = NULL;

fa_device_descriptor_t selected_audio_input_device = NULL;
fa_device_descriptor_t selected_audio_output_device = NULL;
fa_list_t selected_midi_input_devices = NULL;
fa_list_t selected_midi_output_devices = NULL;
fa_echo_type_t selected_midi_echo = FA_ECHO_AUDIO;
fa_device_descriptor_t selected_midi_echo_device = NULL;

fa_audio_device_t current_audio_input_device = NULL;
fa_audio_device_t current_audio_output_device = NULL;
fa_list_t current_midi_input_devices = NULL;
fa_list_t current_midi_output_devices = NULL;
fa_ptr_t current_midi_echo_device = NULL;

fa_audio_stream_t current_audio_stream = NULL;
fa_list_t current_midi_input_streams = NULL;
fa_list_t current_midi_output_streams = NULL;
fa_ptr_t current_midi_echo_stream = NULL;
fa_clock_t current_clock = NULL;

#ifdef _WIN32
double synth_volume   = 1.0;
#else
double synth_volume   = 3.0;
#endif
double audio_volume   = 1.0;
double monitor_volume = 0.0;

#define with_mutex(type, var) type var = 0; fa_thread_mutex_t var ## _mutex = NULL

with_mutex(fa_map_t, playback_semaphores);
with_mutex(fa_map_t, audio_files);
with_mutex(volatile int, time_echo);
with_mutex(volatile int, level_echo);

#define kOutputOffset                   0  // These should be exported from faudio
#define kInputOffset                    8

#define kOutputLeft     (kOutputOffset + 0)
#define kOutputRight    (kOutputOffset + 1)
#define kInputLeft      (kInputOffset + 0)
#define kInputRight     (kInputOffset + 1)
#define kSynthLeft      64
#define kSynthRight     65
#define kAudioLeft      66
#define kAudioRight     67
#define kMonitorLeft    68
#define kMonitorRight   69
#define kLevelLeft      70
#define kLevelRight     71

double last_level[2] = { 0, 0 };

    
// Hack warning!
lo_server server = NULL;
lo_address last_address = NULL;

static inline void init_globals() {
#ifdef _WIN32
    synth_name           = fa_string("fluid"); // NB: this is currently not used, because of limitations in signal_synth
#else
    synth_name           = fa_string("dls");
#endif
    audio_name           = fa_string("buffer");
    record_left_name     = fa_string("rbl");
    record_right_name    = fa_string("rbr");
    
    bundle_actions = fa_list_empty();
    
    playback_semaphores = fa_map_empty();
    playback_semaphores_mutex = fa_thread_create_mutex();
    time_echo_mutex = fa_thread_create_mutex();
    level_echo_mutex = fa_thread_create_mutex();
    audio_files = fa_map_empty();
    audio_files_mutex = fa_thread_create_mutex();
    
    selected_midi_input_devices  = fa_list_empty();
    selected_midi_output_devices = fa_list_empty();
    current_midi_input_devices   = fa_list_empty();
    current_midi_output_devices  = fa_list_empty();
    current_midi_input_streams   = fa_list_empty();
    current_midi_output_streams  = fa_list_empty();
}

static inline void destroy_globals() {
    fa_destroy(bundle_actions);
    
    fa_destroy(playback_semaphores);
    fa_destroy(playback_semaphores_mutex);
    fa_destroy(time_echo_mutex);
    fa_destroy(level_echo_mutex);
    fa_destroy(audio_files);
    fa_destroy(audio_files_mutex);
    
    fa_destroy(synth_name);
    fa_destroy(audio_name);
    fa_destroy(record_left_name);
    fa_destroy(record_right_name);
    
    fa_destroy(selected_midi_input_devices);
    fa_destroy(selected_midi_output_devices);
    fa_destroy(current_midi_input_devices);
    fa_destroy(current_midi_output_devices);
    fa_destroy(current_midi_input_streams);
    fa_destroy(current_midi_output_streams);
}

#endif
