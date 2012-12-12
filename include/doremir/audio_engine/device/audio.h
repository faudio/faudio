
#ifndef _DOREMIR_AUDIOENGINE_DEVICE_AUDIO
#define _DOREMIR_AUDIOENGINE_DEVICE_AUDIO

#include <doremir/list.h>
#include <doremir/pair.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAudioEngine AudioEngine
    @{
    @defgroup DoremirAudioEngineDevice Device
    @{
    @defgroup DoremirAudioEngineDeviceAudio Audio
    @{
    */

typedef struct _doremir_audio_engine_device_audio_session_t * doremir_audio_engine_device_audio_session_t;
typedef struct _doremir_audio_engine_device_audio_device_t * doremir_audio_engine_device_audio_device_t;
typedef struct _doremir_audio_engine_device_audio_stream_t * doremir_audio_engine_device_audio_stream_t;
doremir_audio_engine_device_audio_session_t doremir_audio_engine_device_audio_begin_session();
void doremir_audio_engine_device_audio_end_session(doremir_audio_engine_device_audio_session_t);
doremir_list_t doremir_audio_engine_device_audio_devices(doremir_audio_engine_device_audio_session_t);
doremir_pair_t doremir_audio_engine_device_audio_standard(doremir_audio_engine_device_audio_session_t);
void doremir_audio_engine_device_audio_close_stream(doremir_audio_engine_device_audio_stream_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_DEVICE_AUDIO

