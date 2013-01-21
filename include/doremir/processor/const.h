
#ifndef _DOREMIR_PROCESSOR_CONST
#define _DOREMIR_PROCESSOR_CONST

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorConst Const
    @{
    */

typedef struct _doremir_processor_const_proc_t * doremir_processor_const_proc_t;
doremir_processor_const_proc_t doremir_processor_const_create(doremir_type_t,
                                                              doremir_type_t,
                                                              doremir_ptr_t);
void doremir_processor_const_destroy(doremir_processor_const_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_CONST

