
#ifndef _DOREMIR_PROCESSOR_ID
#define _DOREMIR_PROCESSOR_ID

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorId Id
    @{
    */

typedef struct _doremir_processor_id_proc_t *doremir_processor_id_proc_t;
doremir_processor_id_proc_t doremir_processor_id_create(doremir_type_t);
void doremir_processor_id_destroy(doremir_processor_id_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_ID

