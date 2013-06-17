
#ifndef _FAE_PRIORITYQUEUE
#define _FAE_PRIORITYQUEUE

#include <fae.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaePriorityQueue PriorityQueue
    @{
    */

typedef struct _fae_priority_queue_t * fae_priority_queue_t;
fae_priority_queue_t fae_priority_queue_empty();
fae_priority_queue_t fae_priority_queue_single(fae_ptr_t);
void fae_priority_queue_destroy(fae_priority_queue_t);
void fae_priority_queue_merge(fae_priority_queue_t,
                              fae_priority_queue_t);
void fae_priority_queue_insert(fae_ptr_t, fae_priority_queue_t);
fae_ptr_t fae_priority_queue_peek(fae_priority_queue_t);
fae_ptr_t fae_priority_queue_pop(fae_priority_queue_t);

/** @}
    @}
    */

#endif // _FAE_PRIORITYQUEUE

