
#ifndef _FAE_ATOMIC
#define _FAE_ATOMIC

#include <fae.h>
#include <fae/std.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeAtomic Atomic
    @{
    */

typedef struct _fae_atomic_t * fae_atomic_t;
fae_atomic_t fae_atomic_create();
fae_atomic_t fae_atomic_copy(fae_atomic_t);
void fae_atomic_swap(fae_atomic_t, fae_atomic_t);
void fae_atomic_destroy(fae_atomic_t);
bool fae_atomic_exchange(fae_atomic_t, fae_ptr_t, fae_ptr_t);
fae_ptr_t fae_atomic_get(fae_atomic_t);
void fae_atomic_set(fae_atomic_t, fae_ptr_t);
void fae_atomic_modify(fae_atomic_t, fae_unary_t, fae_ptr_t);

/** @}
    @}
    */

#endif // _FAE_ATOMIC

