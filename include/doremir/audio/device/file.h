
#ifndef _DOREMIR_AUDIO_DEVICE_FILEDEVICE
#define _DOREMIR_AUDIO_DEVICE_FILEDEVICE

#include <Doremir/String.h>

/** @defgroup Doremir
    @{
    @defgroup Audio
    @{
    @defgroup Device
    @{
    @defgroup FileDevice
    @{
    */

typedef struct _doremir_file_device_t * doremir_file_device_t;
typedef struct _doremir_audio_device_file_device_stream_t * doremir_audio_device_file_device_stream_t;
doremir_file_device_t doremir_audio_device_file_device_create(doremir_string_file_path_t,
                                                              doremir_string_file_path_t);
void doremir_audio_device_file_device_destroy(doremir_file_device_t);
void doremir_audio_device_file_device_close_stream(doremir_audio_device_file_device_stream_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIO_DEVICE_FILEDEVICE

