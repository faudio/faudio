
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


typedef void (* fa_map_destructor_t)(fa_ptr_t);

/** Create an empty map. 
*/
fa_map_t fa_map_empty();

/** Copy the given map. 
*/
fa_map_t fa_map_copy(fa_map_t map);

/** Destroy the given map. 
*/
void fa_map_destroy(fa_map_t map);

/** Set the destructor for the given map. (Tip: use fa_destroy)
    
*/
void fa_map_set_value_destructor(fa_map_t map,
                                 fa_map_destructor_t destructor);

/** Return the number of entries in the given map. 
*/
int fa_map_size(fa_map_t map);

/** Return whether the given map is empty. 
*/
bool fa_map_is_empty(fa_map_t map);

/** Return whether the given map has a single entry. 
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

/** Return the element stored at the given key, or `null` if the key does not exist. 
*/
fa_ptr_t fa_map_get(fa_map_key_t key, fa_map_t map);

/** Return the element stored at the given key, or `null` if the key does not exist.
    The key is destroyed. 
*/
fa_ptr_t fa_map_dget(fa_map_key_t key, fa_map_t map);

/** Convert the given list to a map. 
*/
fa_map_t fa_map_from_list(fa_list_t list);

/** Convert the given list to a map. The list is destroyed. 
*/
fa_map_t fa_map_from_list(fa_list_t list);

/** Convert the given map to a list. 
*/
fa_list_t fa_map_to_list(fa_map_t map);

/** Get a list of all keys in the map. The keys are not copied, so don't free them!
*/
fa_list_t fa_map_get_keys(fa_map_t map);


void fa_map_log_count();

/** @}
    @}
    */

#endif // _FA_MAP

