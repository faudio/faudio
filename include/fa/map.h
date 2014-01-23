
#ifndef _FA_MAP
#define _FA_MAP

#include <fa.h>
#include <fa/pair.h>
#include <fa/list.h>
#include <fa/string.h>

/** @addtogroup FaMap
 
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

/** Create an empty list. 
*/
fa_map_t fa_map_empty();

/** Copy the given list. 
*/
fa_map_t fa_map_copy(fa_map_t map);

/** Destroy the given list. 
*/
void fa_map_destroy(fa_map_t map);

/** Return the number of elements in the given list. 
*/
int fa_map_size(fa_map_t map);

/** Return whether the given list is empty. 
*/
bool fa_map_is_empty(fa_map_t map);

/** Return whether the given list has a single element. 
*/
bool fa_map_is_single(fa_map_t map);

/** Add an element to the map if not present. 
*/
fa_map_t fa_map_add(fa_map_key_t key, fa_ptr_t ptr, fa_map_t map);

/** Add an element to the map, replacing if already present. 
*/
fa_map_t fa_map_set(fa_map_key_t key, fa_ptr_t ptr, fa_map_t map);

/** Remove the given key if present. 
*/
fa_map_t fa_map_remove(fa_map_key_t key, fa_map_t map);

/** Add an element to the map if not present. 
*/
fa_map_t fa_map_dadd(fa_map_key_t key, fa_ptr_t ptr, fa_map_t map);

/** Add an element to the map, replacing if already present. 
*/
fa_map_t fa_map_dset(fa_map_key_t key, fa_ptr_t ptr, fa_map_t map);

/** Remove the given key if present. 
*/
fa_map_t fa_map_dremove(fa_map_key_t key, fa_map_t map);

/** Add an element to the map if not present. 
*/
fa_map_t fa_map_add_entry(fa_pair_t pair, fa_map_t map);

/** Add an element to the map, replacing if already present. 
*/
fa_map_t fa_map_set_entry(fa_pair_t pair, fa_map_t map);

/** Remove the given key if present. 
*/
fa_map_t fa_map_remove_entry(fa_pair_t pair, fa_map_t map);

/** Return the element stored at the given key, or `null` if the key does not exist. 
*/
fa_ptr_t fa_map_get(fa_map_key_t key, fa_map_t map);

/** Return whether the given key exists in the given map. 
*/
bool fa_map_has_key(fa_map_key_t key, fa_map_t map);

/** Return whether the given element exists in the given map. 
*/
bool fa_map_has_elem(fa_ptr_t ptr, fa_map_t map);

/** Return whether the given $(key, element)$ pair element exists in the given map. 
*/
bool fa_map_has_entry(fa_pair_t pair, fa_map_t map);

/** Whether the first of the given maps is a submap of the second. 
*/
bool fa_map_is_submap_of(fa_map_t map, fa_map_t map_);

/** Whether the first of the given maps is a proper submap of the second. 
*/
bool fa_map_is_proper_submap_of(fa_map_t map, fa_map_t map_);

/** Sum or union of the given maps. 
*/
fa_map_t fa_map_sum(fa_map_t map, fa_map_t map_);

/** Cartesian product of the given maps. 
*/
fa_map_t fa_map_product(fa_map_t map, fa_map_t map_);

/** Symmetric difference of the given maps. 
*/
fa_map_t fa_map_difference(fa_map_t map, fa_map_t map_);

/** Return the result of applying the given function to all elements of the given list. 
*/
fa_map_t fa_map_map(fa_unary_t unary, fa_ptr_t ptr, fa_map_t map);

/** Convert the given pair to a map. 
*/
fa_map_t fa_map_from_pair(fa_pair_t pair);

/** Convert the given list to a map. 
*/
fa_map_t fa_map_from_list(fa_list_t list);

/** Convert the given map to a list. 
*/
fa_list_t fa_map_to_list(fa_map_t map);

/** @}
    @}
    */

#endif // _FA_MAP

