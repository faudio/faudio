
#ifndef _DOREMIR_AUDIOENGINE_DEVICE_MIDI
#define _DOREMIR_AUDIOENGINE_DEVICE_MIDI

#include <doremir/list.h>
#include <doremir/pair.h>

/** @defgroup Doremir
    @{
    @defgroup AudioEngine
    @{
    @defgroup Device
    @{
    @defgroup Midi
    @{
    */

typedef struct _doremir_audio_engine_device_midi_session_t * doremir_audio_engine_device_midi_session_t;
typedef struct _doremir_midi_t * doremir_midi_t;
typedef struct _doremir_audio_engine_device_midi_stream_t * doremir_audio_engine_device_midi_stream_t;
doremir_audio_engine_device_midi_session_t doremir_audio_engine_device_midi_begin_session();
void doremir_audio_engine_device_midi_end_session(doremir_audio_engine_device_midi_session_t);
doremir_list_t doremir_audio_engine_device_midi_devices(doremir_audio_engine_device_midi_session_t);
doremir_pair_t doremir_audio_engine_device_midi_standard(doremir_audio_engine_device_midi_session_t);
void doremir_audio_engine_device_midi_close_stream(doremir_audio_engine_device_midi_stream_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_DEVICE_MIDI

