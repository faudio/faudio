
#ifndef _FA_ATOMIC_STACK
#define _FA_ATOMIC_STACK

#include <fa.h>

/** @addtogroup FaAtomicStack
 
    Mutable atomic (lock-free) stacks.

    @see 
    - [Data structures](@ref DataStructures)

 
    @defgroup Fa Fa
    @{
    @defgroup FaAtomic Atomic
    @{
    @defgroup FaAtomicStack Stack
    @{
    */


typedef struct _fa_atomic_stack_t * fa_atomic_stack_t;

/** Create a new stack.
    @par Atomicity
        Non-atomic
*/
fa_atomic_stack_t fa_atomic_stack_create();

/** Destroy the given stack.
    @par Atomicity
        Non-atomic
*/
void fa_atomic_stack_destroy(fa_atomic_stack_t);

/** Read a value from the given stack.
    @return
        A value (optional).
    @par Atomicity
        Atomic
*/
fa_ptr_t fa_atomic_stack_read(fa_atomic_stack_t);

/** Write the given value to the given stack.
    @param stackr   Queue.
    @param value    Value to write (optional).
    @par Atomicity
        Atomic
*/
bool fa_atomic_stack_write(fa_atomic_stack_t, fa_ptr_t);

/** @}
    @}
    @}
    */

#endif // _FA_ATOMIC_STACK

