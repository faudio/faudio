
#ifndef _FA_PRIORITYQUEUE
#define _FA_PRIORITYQUEUE

#include <fa.h>

/** @addtogroup FaPriorityQueue

    Priority queue data structure.

    @par Requires
    - fa_order_t
    @par Implements
    - fa_equal_t
    - fa_string_show_t
    - fa_destroy_t

    @see
    - [Data structures](@ref DataStructures)

 
    @defgroup Fa Fa
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

