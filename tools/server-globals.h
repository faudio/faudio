#include <fa/fa.h>




fa_audio_session_t current_audio_session = NULL;
fa_midi_session_t current_midi_session = NULL;
int session_count = 0;

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
