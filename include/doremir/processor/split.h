
#ifndef _DOREMIR_PROCESSOR_DELAY
#define _DOREMIR_PROCESSOR_DELAY

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorDelay Delay
    @{
    */

typedef struct _doremir_processor_delay_proc_t * doremir_processor_delay_proc_t;
doremir_processor_delay_proc_t doremir_processor_delay_create(doremir_type_t,
                                                              size_t);
void doremir_processor_delay_destroy(doremir_processor_delay_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_DELAY

