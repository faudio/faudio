
#ifndef _DOREMIR_ATOMIC_QUEUE
#define _DOREMIR_ATOMIC_QUEUE



/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirAtomic Atomic
    @{
    @defgroup DoremirAtomicQueue Queue
    @{
    */

typedef struct _doremir_atomic_queue_t * doremir_atomic_queue_t;
doremir_atomic_queue_t doremir_atomic_queue_create();
void doremir_atomic_queue_destroy(doremir_atomic_queue_t);
intptr_t doremir_atomic_queue_read(doremir_atomic_queue_t);
intptr_t doremir_atomic_queue_write(doremir_atomic_queue_t,
                                    intptr_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_ATOMIC_QUEUE

