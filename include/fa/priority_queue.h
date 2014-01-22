
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

/**  Create an empty queue. 
*/
fa_priority_queue_t fa_priority_queue_empty();

/**  Create an queue containing a single element. 
*/
fa_priority_queue_t fa_priority_queue_single(fa_ptr_t ptr);

/**  Destroy the given queue. 
*/
void fa_priority_queue_destroy(fa_priority_queue_t priorityQueue);

/**  Merge the two given queues into the first. 
*/
void fa_priority_queue_merge(fa_priority_queue_t priorityQueue,
                             fa_priority_queue_t priorityQueue_);

/**  Insert the given element into the given queue. 
*/
void fa_priority_queue_insert(fa_ptr_t ptr,
                              fa_priority_queue_t priorityQueue);

/**  Returns the top-most element in the given queue, if any.
     @errors
        Returns `NULL` if the queue is empty.
*/
fa_ptr_t fa_priority_queue_peek(fa_priority_queue_t priorityQueue);

/**  Returns the top-most element and remove it from the queue.
     @errors
        Returns `NULL` if the queue is empty.
*/
fa_ptr_t fa_priority_queue_pop(fa_priority_queue_t priorityQueue);

/** @}
    @}
    */

#endif // _FA_PRIORITYQUEUE

