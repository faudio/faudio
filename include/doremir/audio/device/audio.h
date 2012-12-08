
#ifndef _DOREMIR_AUDIO_DEVICE_AUDIODEVICE
#define _DOREMIR_AUDIO_DEVICE_AUDIODEVICE



/** @defgroup Doremir
    @{
    @defgroup Audio
    @{
    @defgroup Device
    @{
    @defgroup AudioDevice
    @{
    */

typedef intptr_t doremir_audio_device_audio_device_t;
typedef intptr_t doremir_audio_device_audio_device_session_t;
typedef intptr_t doremir_audio_device_audio_device_stream_t;
typedef doremir_audio_device_audio_device_list_t doremir_audio_device_audio_device_audio_device_list_t;
doremir_audio_device_audio_device_doremir_audio_device_audiodevice_session_t doremir_audio_device_audio_device_begin_session();
void doremir_audio_device_audio_device_end_session(doremir_audio_device_audio_device_doremir_audio_device_audiodevice_session_t);
doremir_audio_device_audio_device_doremir_audio_device_audiodevice_audio_device_list_t doremir_audio_device_audio_device_devices(doremir_audio_device_audio_device_doremir_audio_device_audiodevice_session_t);
doremir_audio_device_audio_device_audio_device_pair_t doremir_audio_device_audio_device_standard(doremir_audio_device_audio_device_doremir_audio_device_audiodevice_session_t);
doremir_audio_device_audio_device_doremir_audio_device_audiodevice_stream_t doremir_audio_device_audio_device_open_stream(doremir_audio_device_audio_device_doremir_audio_device_audiodevice_audio_device_t,
                                                                                                                          doremir_audio_device_audio_device_proc_t);
void doremir_audio_device_audio_device_close_stream(doremir_audio_device_audio_device_doremir_audio_device_audiodevice_stream_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIO_DEVICE_AUDIODEVICE

