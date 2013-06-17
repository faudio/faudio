
#ifndef _FAE_PROCESSOR_BINARY
#define _FAE_PROCESSOR_BINARY

#include <fae.h>
#include <fae/processor.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeProcessor Processor
    @{
    @defgroup FaeProcessorBinary Binary
    @{
    */

typedef struct _fae_processor_binary_proc_t * fae_processor_binary_proc_t;
fae_processor_binary_proc_t fae_processor_binary_create(fae_type_t,
                                                        fae_type_t,
                                                        fae_type_t,
                                                        fae_binary_t,
                                                        fae_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FAE_PROCESSOR_BINARY

