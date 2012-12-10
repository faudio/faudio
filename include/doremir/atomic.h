
#ifndef _DOREMIR_ATOMIC
#define _DOREMIR_ATOMIC

#include <doremir/std.h>
#include <doremir/bool.h>

/** @defgroup Doremir
    @{
    @defgroup Atomic
    @{
    */

typedef struct _doremir_atomic_t * doremir_atomic_t;
typedef intptr_t (* doremir_atomic_updater_t)(intptr_t);
doremir_atomic_t doremir_atomic_create();
doremir_atomic_t doremir_atomic_copy(doremir_atomic_t);
void doremir_atomic_swap(doremir_atomic_t, doremir_atomic_t);
void doremir_atomic_destroy(doremir_atomic_t);
bool doremir_atomic_exchange(doremir_atomic_t, intptr_t);
void doremir_atomic_add(doremir_atomic_t, intptr_t);
intptr_t doremir_atomic_get(doremir_atomic_t);
void doremir_atomic_modify(doremir_atomic_t,
                           doremir_atomic_updater_t);
void doremir_atomic_set(doremir_atomic_t, intptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_ATOMIC

