
#ifndef _DOREMIR_AUDIOENGINE_DEVICE_BUFFER
#define _DOREMIR_AUDIOENGINE_DEVICE_BUFFER



/** @defgroup Doremir
    @{
    @defgroup AudioEngine
    @{
    @defgroup Device
    @{
    @defgroup Buffer
    @{
    */

typedef struct _doremir_audio_engine_device_buffer_t * doremir_audio_engine_device_buffer_t;
doremir_audio_engine_device_buffer_t doremir_audio_engine_device_buffer_create(size_t);
void doremir_audio_engine_device_buffer_destroy(doremir_audio_engine_device_buffer_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_DEVICE_BUFFER

