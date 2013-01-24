
#ifndef _DOREMIR_PRIORITYQUEUE
#define _DOREMIR_PRIORITYQUEUE

#include <doremir.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPriorityQueue PriorityQueue
    @{
    */

typedef struct _doremir_priority_queue_t * doremir_priority_queue_t;
doremir_priority_queue_t doremir_priority_queue_empty();
doremir_priority_queue_t doremir_priority_queue_single(doremir_ptr_t);
void doremir_priority_queue_destroy(doremir_priority_queue_t);
void doremir_priority_queue_merge(doremir_priority_queue_t,
                                  doremir_priority_queue_t);
void doremir_priority_queue_insert(doremir_ptr_t,
                                   doremir_priority_queue_t);
doremir_ptr_t doremir_priority_queue_peek(doremir_priority_queue_t);
doremir_ptr_t doremir_priority_queue_pop(doremir_priority_queue_t);

/** @}
    @}
    */

#endif // _DOREMIR_PRIORITYQUEUE

