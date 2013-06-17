
#ifndef _FAE_ATOMIC_STACK
#define _FAE_ATOMIC_STACK

#include <fae.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeAtomic Atomic
    @{
    @defgroup FaeAtomicStack Stack
    @{
    */

typedef struct _fae_atomic_stack_t * fae_atomic_stack_t;
fae_atomic_stack_t fae_atomic_stack_create();
void fae_atomic_stack_destroy(fae_atomic_stack_t);
fae_ptr_t fae_atomic_stack_read(fae_atomic_stack_t);
bool fae_atomic_stack_write(fae_atomic_stack_t, fae_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FAE_ATOMIC_STACK

