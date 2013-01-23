
#ifndef _DOREMIR_PROCESSOR_BINARY
#define _DOREMIR_PROCESSOR_BINARY

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorBinary Binary
    @{
    */

typedef struct _doremir_processor_binary_proc_t *doremir_processor_binary_proc_t;
doremir_processor_binary_proc_t doremir_processor_binary_create(doremir_type_t,
    doremir_type_t,
    doremir_type_t,
    doremir_binary_t,
    doremir_ptr_t);
void doremir_processor_binary_destroy(doremir_processor_binary_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_BINARY

