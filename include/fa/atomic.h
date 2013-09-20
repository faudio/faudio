
#ifndef _FA_ATOMIC
#define _FA_ATOMIC

#include <fa.h>
#include <fa/std.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaAtomic Atomic
    @{
    */

typedef struct _fa_atomic_t * fa_atomic_t;
fa_atomic_t fa_atomic_create();
fa_atomic_t fa_atomic_copy(fa_atomic_t);
void fa_atomic_swap(fa_atomic_t, fa_atomic_t);
void fa_atomic_destroy(fa_atomic_t);
bool fa_atomic_exchange(fa_atomic_t, fa_ptr_t, fa_ptr_t);
fa_ptr_t fa_atomic_get(fa_atomic_t);
void fa_atomic_set(fa_atomic_t, fa_ptr_t);
void fa_atomic_modify(fa_atomic_t, fa_unary_t, fa_ptr_t);

/** @}
    @}
    */

#endif // _FA_ATOMIC

