#include "fa/fa.h"

#ifndef __SERVER_TYPES
#define __SERVER_TYPES

typedef uint16_t oid_t;
#define wrap_oid(x) fa_from_int16(x)
#define peek_oid(x) fa_peek_int16(x)

typedef enum {
    AUDIO_DEVICE = 1,
    MIDI_DEVICE  = 2
} fa_device_type_t;

// typedef struct {
//     fa_device_type_t device_type;
//     fa_string_t host;
//     fa_string_t name;
// } *fa_device_descriptor_t;

typedef enum {
    FA_MIDI_NO_OUTPUT = 0,
    FA_MIDI_TO_AUDIO = 1,
    FA_MIDI_TO_DEVICE = 2
} fa_midi_type_t;

typedef enum {
    FA_ECHO_NO_ECHO = 0,
    FA_ECHO_TO_PLAYBACK = 1,
    FA_ECHO_TO_AUDIO = 2,
    FA_ECHO_TO_DEVICE = 3
} fa_echo_type_t;


// typedef struct {
//     lo_address address;
//     lo_server server;
//     oid_t id;
// } playback_context_t;

typedef enum {
    CHOOSE_AUDIO_INPUT_NONE,
    CHOOSE_AUDIO_INPUT_DEFAULT,
    CHOOSE_AUDIO_INPUT_DEVICE,
    CHOOSE_AUDIO_OUTPUT_NONE,
    CHOOSE_AUDIO_OUTPUT_DEFAULT,
    CHOOSE_AUDIO_OUTPUT_DEVICE,
    CHOOSE_MIDI_INPUT_NONE,
    CHOOSE_MIDI_INPUT_ALL,
    CHOOSE_MIDI_INPUT_DEVICE,
    CHOOSE_MIDI_PLAYBACK_NONE,
    CHOOSE_MIDI_PLAYBACK_AUDIO,
    CHOOSE_MIDI_PLAYBACK_DEVICE,
    CHOOSE_MIDI_ECHO_NONE,
    CHOOSE_MIDI_ECHO_PLAYBACK,
    CHOOSE_MIDI_ECHO_AUDIO,
    CHOOSE_MIDI_ECHO_DEVICE,
    CHOOSE_MIDI_ECHO_CHANNEL
} choose_device_t;

typedef enum {
    HOST_SETTINGS_LATENCY,
    HOST_SETTINGS_INPUT_LATENCY,
    HOST_SETTINGS_OUTPUT_LATENCY,
    HOST_SETTINGS_VECTOR_SIZE
} host_setting_t;

typedef enum {
    NOT_RECORDING = 0,
    RECORDING_INITIALIZING,
    RECORDING_RUNNING,
    RECORDING_STOPPING
} recording_state_t;

typedef enum {
    SEQUENCE_STOPPED = 0,
    SEQUENCE_STARTING,
    SEQUENCE_RUNNING,
    SEQUENCE_STOPPING,
    SEQUENCE_FINISHED
} sequence_status_t;

typedef enum {
    NO_STREAM,
    OUTPUT_ONLY,
    INPUT_ONLY,
    BIDIRECTIONAL
} stream_type_t;

#endif
