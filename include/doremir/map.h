
#ifndef _DOREMIR_MAP
#define _DOREMIR_MAP

#include <doremir.h>
#include <doremir/pair.h>
#include <doremir/list.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirMap Map
    @{
    */

typedef struct _doremir_map_t * doremir_map_t;
typedef doremir_ptr_t doremir_map_key_t;
doremir_map_t doremir_map_empty();
doremir_map_t doremir_map_copy(doremir_map_t);
void doremir_map_destroy(doremir_map_t);
int doremir_map_size(doremir_map_t);
bool doremir_map_is_empty(doremir_map_t);
bool doremir_map_is_single(doremir_map_t);
doremir_map_t doremir_map_add(doremir_map_key_t,
                              doremir_ptr_t,
                              doremir_map_t);
doremir_map_t doremir_map_set(doremir_map_key_t,
                              doremir_ptr_t,
                              doremir_map_t);
doremir_map_t doremir_map_remove(doremir_map_key_t, doremir_map_t);
doremir_map_t doremir_map_add_entry(doremir_pair_t, doremir_map_t);
doremir_map_t doremir_map_set_entry(doremir_pair_t, doremir_map_t);
doremir_map_t doremir_map_remove_entry(doremir_pair_t,
                                       doremir_map_t);
doremir_ptr_t doremir_map_get(doremir_map_key_t, doremir_map_t);
bool doremir_map_has_key(doremir_map_key_t, doremir_map_t);
bool doremir_map_has_elem(doremir_ptr_t, doremir_map_t);
bool doremir_map_has_entry(doremir_pair_t, doremir_map_t);
bool doremir_map_is_submap_of(doremir_map_t, doremir_map_t);
bool doremir_map_is_proper_submap_of(doremir_map_t, doremir_map_t);
doremir_map_t doremir_map_sum(doremir_map_t, doremir_map_t);
doremir_map_t doremir_map_product(doremir_map_t, doremir_map_t);
doremir_map_t doremir_map_difference(doremir_map_t, doremir_map_t);
doremir_map_t doremir_map_power(doremir_map_t);
doremir_map_t doremir_map_from_pair(doremir_pair_t);
doremir_map_t doremir_map_from_list(doremir_list_t);
doremir_list_t doremir_map_to_list(doremir_map_t);

/** @}
    @}
    */

#endif // _DOREMIR_MAP

