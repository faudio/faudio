#include <fa/fa.h>

#include <fluidsynth.h>

#ifndef __SERVER_GLOBALS
#define __SERVER_GLOBALS

#define kMaxInMemoryFile 5242880 // 5 MB
#define kFileBufferSize  2097152 // 2 MB
#define kMaxAudioBufferSignals 32
#ifdef _WIN32
#define kDefaultAudioHost "Windows WASAPI"
#else
#define kDefaultAudioHost "Core Audio"
#endif

oid_t last_used_id = 0;
uint32_t ping_counter = 0;
int audio_buffer_signals = 8;
bool verbose = false;

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
#ifdef _WIN32
fa_string_t soundfont_path = NULL;
#endif
fa_string_t default_audio_host = NULL;

#ifdef _WIN32
bool avoid_wasapi_exclusive_bidirectional = true;
#endif
stream_type_t selected_audio_stream_type = BIDIRECTIONAL;
fa_pair_t selected_audio_input_device = NULL;
fa_pair_t selected_audio_output_device = NULL;
fa_list_t selected_midi_input_devices = NULL;
fa_midi_type_t selected_midi_playback = FA_MIDI_TO_AUDIO;
fa_echo_type_t selected_midi_echo = FA_ECHO_TO_PLAYBACK;
fa_pair_t selected_midi_playback_device = NULL;
fa_pair_t selected_midi_echo_device = NULL;

fa_audio_device_t current_audio_input_device = NULL;
fa_audio_device_t current_audio_output_device = NULL;
fa_list_t current_midi_input_devices = NULL;
fa_midi_type_t current_midi_playback = FA_MIDI_TO_AUDIO;
fa_midi_type_t current_midi_echo = FA_MIDI_TO_AUDIO;
fa_ptr_t current_midi_playback_device = NULL;
fa_ptr_t current_midi_echo_device = NULL;

int midi_echo_channel = -1;

stream_type_t current_audio_stream_type = NO_STREAM;
fa_audio_stream_t current_audio_stream = NULL;
fa_list_t current_midi_input_streams   = NULL;
fa_ptr_t current_midi_playback_stream  = NULL;
fa_ptr_t current_midi_echo_stream      = NULL;
fa_clock_t current_clock = NULL;
double current_sample_rate = 0;

fa_atomic_ring_buffer_t recording_ring_buffer = NULL;
fa_thread_t recording_thread = NULL;
// bool recording_flag = false;

fa_map_t session_settings    = NULL;
fa_map_t host_input_latency  = NULL;
fa_map_t host_output_latency = NULL;
fa_map_t host_vector_size    = NULL;

fluid_synth_t *fluid_synth   = NULL;

fa_thread_mutex_t osc_mutex  = NULL;

#ifdef _WIN32
double synth_volume   = 1.0;
#else
double synth_volume   = 3.0;
#endif
double audio_volume   = 1.0;
double monitor_volume = 0.0;

#define with_mutex(type, var) type var = 0; fa_thread_mutex_t var ## _mutex = NULL

with_mutex(recording_state_t, recording_state);
with_mutex(fa_map_t, playback_semaphores);
with_mutex(fa_map_t, recording_semaphores);
with_mutex(fa_map_t, audio_files);
with_mutex(volatile int, time_echo);
with_mutex(volatile int, level_echo);
with_mutex(fa_map_t, uploads);

int time_echo_id  = 0;
int level_echo_id = 0;

#define kRingBufferSize (44100 * 8 * 20) // 20 seconds

#define kOutputOffset   0  // These should be exported from faudio
#define kInputOffset    8

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
    
    recording_state = NOT_RECORDING;
    recording_state_mutex = fa_thread_create_mutex();
    playback_semaphores = fa_map_empty();
    playback_semaphores_mutex = fa_thread_create_mutex();
    recording_semaphores = fa_map_empty();
    recording_semaphores_mutex = fa_thread_create_mutex();
    time_echo_mutex = fa_thread_create_mutex();
    level_echo_mutex = fa_thread_create_mutex();
    audio_files = fa_map_empty();
    audio_files_mutex = fa_thread_create_mutex();
    uploads = fa_map_empty();
    uploads_mutex = fa_thread_create_mutex();
    
    selected_midi_input_devices   = fa_list_empty();
    selected_midi_playback_device = NULL;
    selected_midi_echo_device     = NULL;
    current_midi_input_devices    = fa_list_empty();
    current_midi_playback_device  = NULL;
    current_midi_echo_device      = NULL;
    current_midi_input_streams    = fa_list_empty();
    current_midi_playback_stream  = NULL;
    current_midi_echo_stream      = NULL;
    
    // Using global ring buffer means no parallel recordings, but
    // that is an acceptable limitation for now
    recording_ring_buffer = fa_atomic_ring_buffer(kRingBufferSize);
    
    session_settings    = fa_map_empty();
    host_input_latency  = fa_map_empty();
    host_output_latency = fa_map_empty();
    host_vector_size    = fa_map_empty();
    session_settings    = fa_map_empty();
    fa_map_set_value_destructor(session_settings, fa_destroy);
    fa_map_set_value_destructor(host_input_latency, fa_destroy);
    fa_map_set_value_destructor(host_output_latency, fa_destroy);
    fa_map_set_value_destructor(host_vector_size, fa_destroy);
    
    osc_mutex = fa_thread_create_mutex();
}

static inline void destroy_globals() {
    fa_slog_info("Destroying bundle_actions...");
    fa_destroy(bundle_actions);
    
    fa_slog_info("Destroying mutex...");
    fa_destroy(recording_state_mutex);
    fa_destroy(playback_semaphores);
    fa_destroy(playback_semaphores_mutex);
    fa_destroy(recording_semaphores);
    fa_destroy(recording_semaphores_mutex);
    fa_destroy(time_echo_mutex);
    fa_destroy(level_echo_mutex);
    fa_destroy(audio_files);
    fa_destroy(audio_files_mutex);
    fa_destroy(uploads);
    fa_destroy(uploads_mutex);
    
    fa_slog_info("Destroying strings...");
    fa_destroy(synth_name);
    fa_destroy(audio_name);
    fa_destroy(record_left_name);
    fa_destroy(record_right_name);
    
    fa_slog_info("Destroying device references...");
    if (selected_midi_input_devices) fa_destroy(selected_midi_input_devices);
    fa_destroy(current_midi_input_devices);
    fa_destroy(current_midi_input_streams);
    
    fa_slog_info("Destroying ring buffer...");
    fa_destroy(recording_ring_buffer);
    
    fa_slog_info("Destroying settings...");
    fa_destroy(session_settings);
    fa_destroy(host_input_latency);
    fa_destroy(host_output_latency);
    fa_destroy(host_vector_size);
    
    fa_destroy(osc_mutex);
}

#endif
