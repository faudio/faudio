
#ifndef _FAE_PROCESSOR_DELAY
#define _FAE_PROCESSOR_DELAY

#include <fae.h>
#include <fae/processor.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeProcessor Processor
    @{
    @defgroup FaeProcessorDelay Delay
    @{
    */

typedef struct _fae_processor_delay_proc_t * fae_processor_delay_proc_t;
fae_processor_delay_proc_t fae_processor_delay_create(fae_type_t,
                                                      size_t);

/** @}
    @}
    @}
    */

#endif // _FAE_PROCESSOR_DELAY

