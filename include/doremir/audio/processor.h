
#ifndef _DOREMIR_AUDIO_PROCESSOR
#define _DOREMIR_AUDIO_PROCESSOR

#include <doremir/std.h>
#include <doremir/buffer.h>

/** @defgroup Doremir
    @{
    @defgroup Audio
    @{
    @defgroup Processor
    @{
    */

typedef doremir_buffer_t doremir_audio_processor_samples_t;
typedef struct {
            doremir_audio_processor_samples_t (* process)(doremir_audio_processor_samples_t);
        } doremir_processor_t;

/** @}
    @}
    @}
    */

#endif // _DOREMIR_AUDIO_PROCESSOR

