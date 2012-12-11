
#ifndef _DOREMIR_AUDIOENGINE_PROCESSOR
#define _DOREMIR_AUDIOENGINE_PROCESSOR

#include <doremir/std.h>
#include <doremir/buffer.h>

/** @defgroup Doremir
    @{
    @defgroup AudioEngine
    @{
    @defgroup Processor
    @{
    */

typedef doremir_buffer_t doremir_audio_engine_processor_samples_t;
typedef struct {
            doremir_audio_engine_processor_samples_t (* process)(doremir_audio_engine_processor_samples_t);
        } doremir_processor_t;

/** @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIOENGINE_PROCESSOR

