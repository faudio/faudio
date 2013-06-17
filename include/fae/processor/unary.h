
#ifndef _FAE_PROCESSOR_UNARY
#define _FAE_PROCESSOR_UNARY

#include <fae.h>
#include <fae/processor.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeProcessor Processor
    @{
    @defgroup FaeProcessorUnary Unary
    @{
    */

typedef struct _fae_processor_unary_proc_t * fae_processor_unary_proc_t;
fae_processor_unary_proc_t fae_processor_unary_create(fae_type_t,
                                                      fae_type_t,
                                                      fae_unary_t,
                                                      fae_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FAE_PROCESSOR_UNARY

