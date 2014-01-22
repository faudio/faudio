
#ifndef _FA_PAIR
#define _FA_PAIR

#include <fa.h>
#include <fa/list.h>

/** @addtogroup FaPair
 
    Immutable pair data structure.

    @par Literals
    - `pair(1, 2)`

    @par Implements 
    - fa_equal_t
    - fa_order_t
    - fa_copy_t
    - fa_destroy_t
    - fa_dynamic_t
    - fa_string_show_t

    @see 
    - [Data structures](@ref DataStructures)

 
    @defgroup Fa Fa
    @{
    @defgroup FaPair Pair
    @{
    */

/** The abstract type of pairs.
*/
typedef struct _fa_pair_t * fa_pair_t;


typedef struct {
            fa_ptr_t first; fa_ptr_t second;
        } fa_pair_struct_t;

/** Create a new pair.
*/
fa_pair_t fa_pair_create(fa_ptr_t ptr, fa_ptr_t ptr_);

/** Create a pair by reading the components of a structure.
*/
fa_pair_t fa_pair_read(fa_pair_struct_t *);

/** Write the values of a pair to a structure.
*/
void fa_pair_write(fa_pair_struct_t *, fa_pair_t pair);

/** Copy the given pair.
*/
fa_pair_t fa_pair_copy(fa_pair_t pair);

/** Destroy the given pair.
*/
void fa_pair_destroy(fa_pair_t pair);

/** Get the first and second components of the given pair.
*/
void fa_pair_decons(fa_ptr_t *, fa_ptr_t *, fa_pair_t pair);

/** Get the first component of the given pair.
*/
fa_ptr_t fa_pair_first(fa_pair_t pair);

/** Get the second component of the given pair.
*/
fa_ptr_t fa_pair_second(fa_pair_t pair);

/** Return a pair containing the given value as both its left and right component.
*/
fa_pair_t fa_pair_duplicate(fa_ptr_t ptr);

/** Swap the components of the given pair.
*/
fa_pair_t fa_pair_swap(fa_pair_t pair);

/** Return the left-associated version of the given nested pair.
*/
fa_pair_t fa_pair_assoc(fa_pair_t pair);

/** Return the right-associated version of the given nested pair.
*/
fa_pair_t fa_pair_unassoc(fa_pair_t pair);

/** Convert a pair to a list of two elements.
*/
fa_list_t fa_pair_to_list(fa_pair_t pair);

/** @}
    @}
    */

#endif // _FA_PAIR

