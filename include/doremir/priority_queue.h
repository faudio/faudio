
#ifndef _DOREMIR_PRIORITYQUEUE
#define _DOREMIR_PRIORITYQUEUE

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPriorityQueue PriorityQueue
    @{
    */

typedef struct _doremir_priority_queue_t * doremir_priority_queue_t;
typedef intptr_t doremir_priority_queue_value_t;
doremir_priority_queue_t doremir_priority_queue_create();
void doremir_priority_queue_swap(doremir_priority_queue_t,
                                 doremir_priority_queue_t);
void doremir_priority_queue_destroy(doremir_priority_queue_t);
void doremir_priority_queue_insert(doremir_priority_queue_t,
                                   doremir_priority_queue_value_t);
doremir_priority_queue_value_t doremir_priority_queue_peek(doremir_priority_queue_t);
doremir_priority_queue_value_t doremir_priority_queue_pop(doremir_priority_queue_t);

/** @}
    @}
    */

#endif // _DOREMIR_PRIORITYQUEUE

