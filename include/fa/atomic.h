
#ifndef _FA_ATOMIC
#define _FA_ATOMIC

#include <fa.h>
#include <fa/std.h>

/** @addtogroup FaAtomic

    Mutable atomic references.

    @par Literals
    - `atomic()`

    @par Implements
    - fa_equal_t
    - fa_order_t
    - fa_copy_t
    - fa_destroy_t
    - fa_string_show_t

    @see
    - [Data structures](@ref DataStructures)
 
    @defgroup Fa Fa
    @{
    @defgroup FaAtomic Atomic
    @{
    */


typedef struct _fa_atomic_t * fa_atomic_t;

/** Create a new atomic reference.
    @par Atomicity
        Non-atomic
*/
fa_atomic_t fa_atomic_create();

/** Copy the given atomic reference.
    @par Atomicity
        Non-atomic
*/
fa_atomic_t fa_atomic_copy(fa_atomic_t);

/** Destroy the given atomic reference.
    @par Atomicity
        Non-atomic
*/
void fa_atomic_destroy(fa_atomic_t);

/** Compares the given value with the current value of the given atomic reference,
    replacing it if successful.

    @param a   The atomic reference.
    @param old Old value.
    @param new New value.
    @return
        Whether comparison was successful, i.e. whether the values where exchanged.
    @par Atomicity
        Atomic
*/
bool fa_atomic_exchange(fa_atomic_t, fa_ptr_t, fa_ptr_t);

/** Return the current value of the given atomic reference.
    @par Atomicity
        Atomic
*/
fa_ptr_t fa_atomic_get(fa_atomic_t);

/** Update the given atomic value by applying the given pure function.

    @param atomic   Atomic reference.
    @param func     Function to be applied to the value.
    @param data     Value to be passed to the function.

    @par Atomicity Atomic
*/
void fa_atomic_set(fa_atomic_t, fa_ptr_t);

/** Set the given given atomic reference.

    @par Atomicity Atomic
*/
void fa_atomic_modify(fa_atomic_t, fa_unary_t, fa_ptr_t);

/** @}
    @}
    */

#endif // _FA_ATOMIC

