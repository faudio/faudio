
#ifndef _DOREMIR_AUDIOENGINE_DEVICE_BUFFERED
#define _DOREMIR_AUDIOENGINE_DEVICE_BUFFERED



/** @defgroup Doremir
    @{
    @defgroup AudioEngine
    @{
    @defgroup Device
    @{
    @defgroup Buffered
    @{
    */

typedef struct _doremir_buffered_t * doremir_buffered_t;
doremir_buffered_t doremir_audio_engine_device_buffered_create(size_t);
void doremir_audio_engine_device_buffered_destroy(doremir_buffered_t);

/** @}
    @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_DEVICE_BUFFERED

