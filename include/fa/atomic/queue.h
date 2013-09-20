
#ifndef _FA_ATOMIC_QUEUE
#define _FA_ATOMIC_QUEUE

#include <fa.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaAtomic Atomic
    @{
    @defgroup FaAtomicQueue Queue
    @{
    */

typedef struct _fa_atomic_queue_t * fa_atomic_queue_t;
fa_atomic_queue_t fa_atomic_queue_create();
void fa_atomic_queue_destroy(fa_atomic_queue_t);
fa_ptr_t fa_atomic_queue_read(fa_atomic_queue_t);
bool fa_atomic_queue_write(fa_atomic_queue_t, fa_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FA_ATOMIC_QUEUE

