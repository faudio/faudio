
#ifndef _FA_MAP
#define _FA_MAP

#include <fa.h>
#include <fa/pair.h>
#include <fa/list.h>
#include <fa/string.h>

/** @addtogroup FaMap
 
    @addtogroup FaMap
    
    Immutable ordered map data structure.

    @par Literals
    - `map(string("name"),           string("hans"), 
           string("favouriteColor"), string("blue"))`

    @par Requires 
    - fa_order_t

    @par Implements 
    - fa_equal_t
    - fa_copy_t
    - fa_destroy_t
    - fa_dynamic_t
    - fa_string_show_t

    @see 
    - [Data structures](@ref DataStructures)
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaMap Map
    @{
    */


typedef struct _fa_map_t * fa_map_t;


typedef fa_ptr_t fa_map_key_t;


fa_map_t fa_map_empty();


fa_map_t fa_map_copy(fa_map_t);


void fa_map_destroy(fa_map_t);


int fa_map_size(fa_map_t);


bool fa_map_is_empty(fa_map_t);


bool fa_map_is_single(fa_map_t);


fa_map_t fa_map_add(fa_map_key_t, fa_ptr_t, fa_map_t);


fa_map_t fa_map_set(fa_map_key_t, fa_ptr_t, fa_map_t);


fa_map_t fa_map_remove(fa_map_key_t, fa_map_t);


fa_map_t fa_map_dadd(fa_map_key_t, fa_ptr_t, fa_map_t);


fa_map_t fa_map_dset(fa_map_key_t, fa_ptr_t, fa_map_t);


fa_map_t fa_map_dremove(fa_map_key_t, fa_map_t);


fa_map_t fa_map_add_entry(fa_pair_t, fa_map_t);


fa_map_t fa_map_set_entry(fa_pair_t, fa_map_t);


fa_map_t fa_map_remove_entry(fa_pair_t, fa_map_t);


fa_ptr_t fa_map_get(fa_map_key_t, fa_map_t);


bool fa_map_has_key(fa_map_key_t, fa_map_t);


bool fa_map_has_elem(fa_ptr_t, fa_map_t);


bool fa_map_has_entry(fa_pair_t, fa_map_t);


bool fa_map_is_submap_of(fa_map_t, fa_map_t);


bool fa_map_is_proper_submap_of(fa_map_t, fa_map_t);


fa_map_t fa_map_sum(fa_map_t, fa_map_t);


fa_map_t fa_map_product(fa_map_t, fa_map_t);


fa_map_t fa_map_difference(fa_map_t, fa_map_t);


fa_map_t fa_map_map(fa_unary_t, fa_ptr_t, fa_map_t);


fa_map_t fa_map_from_pair(fa_pair_t);


fa_map_t fa_map_from_list(fa_list_t);


fa_list_t fa_map_to_list(fa_map_t);

/** @}
    @}
    */

#endif // _FA_MAP

