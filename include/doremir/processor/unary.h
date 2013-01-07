
#ifndef _DOREMIR_PROCESSOR_UNARY
#define _DOREMIR_PROCESSOR_UNARY

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorUnary Unary
    @{
    */

typedef struct _doremir_processor_unary_proc_t * doremir_processor_unary_proc_t;
doremir_processor_unary_proc_t doremir_processor_unary_create(doremir_type_t,
                                                              doremir_unary_t);
void doremir_processor_unary_destroy(doremir_processor_unary_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_UNARY

