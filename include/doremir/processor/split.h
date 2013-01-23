
#ifndef _DOREMIR_PROCESSOR_SPLIT
#define _DOREMIR_PROCESSOR_SPLIT

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorSplit Split
    @{
    */

typedef struct _doremir_processor_split_proc_t *doremir_processor_split_proc_t;
doremir_processor_split_proc_t doremir_processor_split_create(doremir_type_t);
void doremir_processor_split_destroy(doremir_processor_split_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_SPLIT

