#include <fa/fa.h>

#ifndef __SERVER_GLOBALS
#define __SERVER_GLOBALS

fa_audio_session_t current_audio_session = NULL;
fa_midi_session_t current_midi_session = NULL;
int session_count = 0;

fa_string_t synth_name = NULL;
fa_string_t audio_name = NULL;

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

fa_map_t playback_semaphores = NULL;
fa_thread_mutex_t playback_semaphore_mutex = NULL;

fa_map_t audio_files = NULL;
fa_thread_mutex_t audio_files_mutex = NULL;

int time_echo = 0;
fa_thread_mutex_t time_echo_mutex = NULL;
    
// Hack warning!
lo_server server = NULL;
lo_address last_address = NULL;

static inline void init_globals() {
    playback_semaphores = fa_map_empty();
    playback_semaphore_mutex = fa_thread_create_mutex();
    time_echo_mutex = fa_thread_create_mutex();
    audio_files = fa_map_empty();
    audio_files_mutex = fa_thread_create_mutex();
}

static inline void destroy_globals() {
    fa_destroy(playback_semaphores);
    fa_destroy(playback_semaphore_mutex);
    fa_destroy(time_echo_mutex);
    fa_destroy(audio_files);
    fa_destroy(audio_files_mutex);
}

#endif
