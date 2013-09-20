
#ifndef _FA_PRIORITYQUEUE
#define _FA_PRIORITYQUEUE

#include <fa.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaPriorityQueue PriorityQueue
    @{
    */

typedef struct _fa_priority_queue_t * fa_priority_queue_t;
fa_priority_queue_t fa_priority_queue_empty();
fa_priority_queue_t fa_priority_queue_single(fa_ptr_t);
void fa_priority_queue_destroy(fa_priority_queue_t);
void fa_priority_queue_merge(fa_priority_queue_t,
                             fa_priority_queue_t);
void fa_priority_queue_insert(fa_ptr_t, fa_priority_queue_t);
fa_ptr_t fa_priority_queue_peek(fa_priority_queue_t);
fa_ptr_t fa_priority_queue_pop(fa_priority_queue_t);

/** @}
    @}
    */

#endif // _FA_PRIORITYQUEUE

