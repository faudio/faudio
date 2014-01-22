
#ifndef _FA_SET
#define _FA_SET

#include <fa.h>
#include <fa/list.h>
#include <fa/string.h>

/** @addtogroup FaSet
 
    Immutable ordered set data structure.

    @par Literals
    - `set(1, 2, 3)`

    @par Display
    - `{1,2,3}`
    
    @par Requires 
    - fa_order_t

    @par Implements 
    - fa_equal_t
    - fa_string_show_t
    - fa_copy_t
    - fa_destroy_t
    - fa_dynamic_t

    @see 
    - [Data structures](@ref DataStructures)
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaSet Set
    @{
    */

/** The abstract type of sets.
*/
typedef struct _fa_set_t * fa_set_t;


fa_set_t fa_set_empty();


fa_set_t fa_set_single(fa_ptr_t ptr);


fa_set_t fa_set_add(fa_ptr_t ptr, fa_set_t set);


fa_set_t fa_set_set(fa_ptr_t ptr, fa_set_t set);


fa_set_t fa_set_remove(fa_ptr_t ptr, fa_set_t set);


fa_set_t fa_set_dadd(fa_ptr_t ptr, fa_set_t set);


fa_set_t fa_set_dset(fa_ptr_t ptr, fa_set_t set);


fa_set_t fa_set_dremove(fa_ptr_t ptr, fa_set_t set);


fa_set_t fa_set_copy(fa_set_t set);


void fa_set_destroy(fa_set_t set);


int fa_set_size(fa_set_t set);


bool fa_set_is_empty(fa_set_t set);


bool fa_set_is_single(fa_set_t set);


bool fa_set_has(fa_ptr_t ptr, fa_set_t set);


fa_ptr_t fa_set_get(fa_ptr_t ptr, fa_set_t set);


bool fa_set_is_subset_of(fa_set_t set, fa_set_t set);


bool fa_set_is_proper_subset_of(fa_set_t set, fa_set_t set);


fa_set_t fa_set_sum(fa_set_t set, fa_set_t set);


fa_set_t fa_set_intersection(fa_set_t set, fa_set_t set);


fa_set_t fa_set_difference(fa_set_t set, fa_set_t set);


fa_set_t fa_set_product(fa_set_t set, fa_set_t set);


fa_set_t fa_set_from_list(fa_list_t list);


fa_list_t fa_set_to_list(fa_set_t set);

/** @}
    @}
    */

#endif // _FA_SET

