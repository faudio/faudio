
#ifndef _DOREMIR_PROCESSOR_PAR
#define _DOREMIR_PROCESSOR_PAR

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorPar Par
    @{
    */

typedef struct _doremir_processor_par_proc_t * doremir_processor_par_proc_t;
doremir_processor_par_proc_t doremir_processor_par_create(doremir_processor_t,
                                                          doremir_processor_t);
void doremir_processor_par_destroy(doremir_processor_par_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_PAR

