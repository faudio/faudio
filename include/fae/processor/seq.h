
#ifndef _FAE_PROCESSOR_SEQ
#define _FAE_PROCESSOR_SEQ

#include <fae.h>
#include <fae/processor.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeProcessor Processor
    @{
    @defgroup FaeProcessorSeq Seq
    @{
    */

typedef struct _fae_processor_seq_proc_t * fae_processor_seq_proc_t;
fae_processor_seq_proc_t fae_processor_seq_create(fae_processor_t,
                                                  fae_processor_t);

/** @}
    @}
    @}
    */

#endif // _FAE_PROCESSOR_SEQ

