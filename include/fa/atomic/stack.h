
#ifndef _FA_ATOMIC_STACK
#define _FA_ATOMIC_STACK

#include <fa.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaAtomic Atomic
    @{
    @defgroup FaAtomicStack Stack
    @{
    */

typedef struct _fa_atomic_stack_t * fa_atomic_stack_t;
fa_atomic_stack_t fa_atomic_stack_create();
void fa_atomic_stack_destroy(fa_atomic_stack_t);
fa_ptr_t fa_atomic_stack_read(fa_atomic_stack_t);
bool fa_atomic_stack_write(fa_atomic_stack_t, fa_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FA_ATOMIC_STACK

