
#ifndef _DOREMIR_ATOMIC_QUEUE
#define _DOREMIR_ATOMIC_QUEUE

#include <doremir.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAtomic Atomic
    @{
    @defgroup DoremirAtomicQueue Queue
    @{
    */

typedef struct _doremir_atomic_queue_t *doremir_atomic_queue_t;
doremir_atomic_queue_t doremir_atomic_queue_create();
void doremir_atomic_queue_destroy(doremir_atomic_queue_t);
doremir_ptr_t doremir_atomic_queue_read(doremir_atomic_queue_t);
bool doremir_atomic_queue_write(doremir_atomic_queue_t,
                                doremir_ptr_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_ATOMIC_QUEUE

