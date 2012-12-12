
#ifndef _DOREMIR_DEVICE_AUDIO
#define _DOREMIR_DEVICE_AUDIO

#include <doremir/list.h>
#include <doremir/pair.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDevice Device
    @{
    @defgroup DoremirDeviceAudio Audio
    @{
    */

typedef struct _doremir_device_audio_session_t * doremir_device_audio_session_t;
typedef struct _doremir_device_audio_stream_t * doremir_device_audio_stream_t;
typedef struct _doremir_device_audio_t * doremir_device_audio_t;
doremir_device_audio_session_t doremir_device_audio_begin_session();
void doremir_device_audio_end_session(doremir_device_audio_session_t);
doremir_list_t doremir_device_audio_devices(doremir_device_audio_session_t);
doremir_pair_t doremir_device_audio_standard(doremir_device_audio_session_t);
doremir_device_audio_t doremir_device_audio_standard_input(doremir_device_audio_session_t);
doremir_device_audio_t doremir_device_audio_standard_output(doremir_device_audio_session_t);
doremir_device_audio_stream_t doremir_device_audio_open_stream(doremir_device_audio_t,
                                                               doremir_processor_t,
                                                               doremir_device_audio_t);
void doremir_device_audio_close_stream(doremir_device_audio_stream_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_DEVICE_AUDIO

