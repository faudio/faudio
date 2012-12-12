
#ifndef _DOREMIR_PROCESSOR
#define _DOREMIR_PROCESSOR

#include <doremir/std.h>
#include <doremir/buffer.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    */

typedef doremir_buffer_t doremir_processor_samples_t;
typedef struct {
            doremir_processor_samples_t (* process)(doremir_processor_samples_t);
        } doremir_processor_t;

/** @}
    @}
    */

#endif // _DOREMIR_PROCESSOR

