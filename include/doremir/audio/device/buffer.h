
#ifndef _DOREMIR_AUDIO_DEVICE_BUFFERDEVICE
#define _DOREMIR_AUDIO_DEVICE_BUFFERDEVICE

#include <Doremir/Audio.h>

/** @defgroup Doremir
    @{
    @defgroup Audio
    @{
    @defgroup Device
    @{
    @defgroup BufferDevice
    @{
    */

typedef struct _doremir_buffer_device_t * doremir_buffer_device_t;
doremir_buffer_device_t doremir_audio_device_buffer_device_create(size_t,
                                                                  doremir_audio_type_t);
void doremir_audio_device_buffer_device_destroy(doremir_buffer_device_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIO_DEVICE_BUFFERDEVICE

