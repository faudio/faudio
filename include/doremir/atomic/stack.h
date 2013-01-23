
#ifndef _DOREMIR_ATOMIC_STACK
#define _DOREMIR_ATOMIC_STACK

#include <doremir.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAtomic Atomic
    @{
    @defgroup DoremirAtomicStack Stack
    @{
    */

typedef struct _doremir_atomic_stack_t *doremir_atomic_stack_t;
doremir_atomic_stack_t doremir_atomic_stack_create();
void doremir_atomic_stack_destroy(doremir_atomic_stack_t);
doremir_ptr_t doremir_atomic_stack_read(doremir_atomic_stack_t);
bool doremir_atomic_stack_write(doremir_atomic_stack_t,
                                doremir_ptr_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_ATOMIC_STACK

