
#ifndef _DOREMIR_DEVICE_AUDIO
#define _DOREMIR_DEVICE_AUDIO

#include <doremir/list.h>
#include <doremir/pair.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDevice Device
    @{
    @defgroup DoremirDeviceAudio Audio
    @{
    */

typedef struct _doremir_device_audio_session_t * doremir_device_audio_session_t;
typedef struct _doremir_device_audio_device_t * doremir_device_audio_device_t;
typedef struct _doremir_device_audio_stream_t * doremir_device_audio_stream_t;
doremir_device_audio_session_t doremir_device_audio_begin_session();
void doremir_device_audio_end_session(doremir_device_audio_session_t);
doremir_list_t doremir_device_audio_devices(doremir_device_audio_session_t);
doremir_pair_t doremir_device_audio_standard(doremir_device_audio_session_t);
void doremir_device_audio_close_stream(doremir_device_audio_stream_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_DEVICE_AUDIO

