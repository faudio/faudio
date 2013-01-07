
#ifndef _DOREMIR_PROCESSOR_SEQ
#define _DOREMIR_PROCESSOR_SEQ

#include <doremir.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    @defgroup DoremirProcessorSeq Seq
    @{
    */

typedef struct _doremir_processor_seq_proc_t * doremir_processor_seq_proc_t;
doremir_processor_seq_proc_t doremir_processor_seq_create(doremir_processor_any_t,
                                                          doremir_processor_any_t);
void doremir_processor_seq_destroy(doremir_processor_seq_proc_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_PROCESSOR_SEQ

