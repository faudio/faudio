
#ifndef _FAE_PROCESSOR_PAR
#define _FAE_PROCESSOR_PAR

#include <fae.h>
#include <fae/processor.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeProcessor Processor
    @{
    @defgroup FaeProcessorPar Par
    @{
    */

typedef struct _fae_processor_par_proc_t * fae_processor_par_proc_t;
fae_processor_par_proc_t fae_processor_par_create(fae_processor_t,
                                                  fae_processor_t);

/** @}
    @}
    @}
    */

#endif // _FAE_PROCESSOR_PAR

