
#ifndef _DOREMIR_ATOMIC_QUEUE
#define _DOREMIR_ATOMIC_QUEUE



/** @defgroup Doremir
    @{
    @defgroup Atomic
    @{
    @defgroup Queue
    @{
    */

typedef intptr_t doremir_atomic_queue_t;
doremir_atomic_queue_doremir_atomic_queue_queue_t doremir_atomic_queue_create();
void doremir_atomic_queue_destroy(doremir_atomic_queue_doremir_atomic_queue_queue_t);
intptr_t doremir_atomic_queue_read(doremir_atomic_queue_doremir_atomic_queue_queue_t);
intptr_t doremir_atomic_queue_write(doremir_atomic_queue_doremir_atomic_queue_queue_t,
                                    intptr_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_ATOMIC_QUEUE

