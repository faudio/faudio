
#ifndef _FAE_ATOMIC_QUEUE
#define _FAE_ATOMIC_QUEUE

#include <fae.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeAtomic Atomic
    @{
    @defgroup FaeAtomicQueue Queue
    @{
    */

typedef struct _fae_atomic_queue_t * fae_atomic_queue_t;
fae_atomic_queue_t fae_atomic_queue_create();
void fae_atomic_queue_destroy(fae_atomic_queue_t);
fae_ptr_t fae_atomic_queue_read(fae_atomic_queue_t);
bool fae_atomic_queue_write(fae_atomic_queue_t, fae_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FAE_ATOMIC_QUEUE

