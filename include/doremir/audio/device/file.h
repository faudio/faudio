
#ifndef _DOREMIR_AUDIO_DEVICE_FILEDEVICE
#define _DOREMIR_AUDIO_DEVICE_FILEDEVICE



/** @defgroup Doremir
    @{
    @defgroup Audio
    @{
    @defgroup Device
    @{
    @defgroup FileDevice
    @{
    */

typedef intptr_t doremir_audio_device_file_device_t;
doremir_audio_device_file_device_doremir_audio_device_filedevice_file_device_t doremir_audio_device_file_device_create(doremir_audio_device_file_device_file_path_t,
                                                                                                                       doremir_audio_device_file_device_file_path_t);
void doremir_audio_device_file_device_destroy(doremir_audio_device_file_device_doremir_audio_device_filedevice_file_device_t);
doremir_audio_device_file_device_stream_t doremir_audio_device_file_device_open_stream(doremir_audio_device_file_device_doremir_audio_device_filedevice_file_device_t,
                                                                                       doremir_audio_device_file_device_proc_t);
void doremir_audio_device_file_device_close_stream(doremir_audio_device_file_device_stream_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIO_DEVICE_FILEDEVICE

