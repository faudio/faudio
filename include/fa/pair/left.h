
#ifndef _FA_PAIR_LEFT
#define _FA_PAIR_LEFT

#include <fa.h>
#include <fa/pair.h>
#include <fa/list.h>

/** @addtogroup FaPairLeft
 
    Immutable left-leaning pair data structure.
    
    This type is identical to @ref fa_pair_t, except that it compares on the first element.
    Useful for ordered lists etc.

    @par Literals
    - `pair_left(1, 2)`

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
    @defgroup FaPair Pair
    @{
    @defgroup FaPairLeft Left
    @{
    */


typedef struct _fa_pair_left_t * fa_pair_left_t;


typedef struct {
            fa_ptr_t first; fa_ptr_t second;
        } fa_pair_left_struct_t;

/** Create a new pair.
*/
fa_pair_left_t fa_pair_left_create(fa_ptr_t, fa_ptr_t);

/** Create a pair by reading the components of a structure.
*/
fa_pair_left_t fa_pair_left_read(fa_pair_left_struct_t *);

/** Write the values of a pair to a structure.
*/
void fa_pair_left_write(fa_pair_left_struct_t *, fa_pair_left_t);

/** Copy the given pair.
*/
fa_pair_left_t fa_pair_left_copy(fa_pair_left_t);

/** Destroy the given pair.
*/
void fa_pair_left_destroy(fa_pair_left_t);


void fa_pair_left_decons(fa_ptr_t *, fa_ptr_t *, fa_pair_left_t);


fa_pair_t fa_pair_left_to_pair(fa_pair_left_t);


fa_pair_left_t fa_pair_left_from_pair(fa_pair_t);


fa_list_t fa_pair_left_to_list(fa_pair_left_t);

/** @}
    @}
    @}
    */

#endif // _FA_PAIR_LEFT

