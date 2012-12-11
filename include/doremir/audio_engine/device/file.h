
#ifndef _DOREMIR_AUDIOENGINE_DEVICE_FILE
#define _DOREMIR_AUDIOENGINE_DEVICE_FILE

#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAudioEngine AudioEngine
    @{
    @defgroup DoremirAudioEngineDevice Device
    @{
    @defgroup DoremirAudioEngineDeviceFile File
    @{
    */

typedef struct _doremir_audio_engine_device_file_t * doremir_audio_engine_device_file_t;
typedef struct _doremir_audio_engine_device_file_stream_t * doremir_audio_engine_device_file_stream_t;
doremir_audio_engine_device_file_t doremir_audio_engine_device_file_create(doremir_string_file_path_t,
                                                                           doremir_string_file_path_t);
void doremir_audio_engine_device_file_destroy(doremir_audio_engine_device_file_t);
void doremir_audio_engine_device_file_close_stream(doremir_audio_engine_device_file_stream_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_DEVICE_FILE

