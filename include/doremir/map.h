
#ifndef _DOREMIR_MAP
#define _DOREMIR_MAP

#include <doremir/std.h>
#include <doremir/pair.h>

/** @defgroup Doremir
    @{
    @defgroup Map
    @{
    */

typedef struct _doremir_map_t * doremir_map_t;
typedef intptr_t doremir_map_key_t;
typedef intptr_t doremir_map_value_t;
doremir_map_t doremir_map_empty();
doremir_map_t doremir_map_single(doremir_map_key_t,
                                 doremir_map_value_t);
doremir_map_t doremir_map_append(doremir_map_t, doremir_map_t);
void doremir_map_destroy(doremir_map_t);
doremir_map_t doremir_map_copy(doremir_map_t);
void doremir_map_swap(doremir_map_t, doremir_map_t);
doremir_map_t doremir_map_from_pair(doremir_pair_t);
bool doremir_map_key(doremir_map_t, doremir_map_key_t);
bool doremir_map_elem(doremir_map_t, doremir_map_value_t);
bool doremir_map_entry(doremir_map_t, doremir_pair_t);
bool doremir_map_submap_of(doremir_map_t, doremir_map_t);
bool doremir_map_proper_submap_of(doremir_map_t, doremir_map_t);
doremir_map_t doremir_map_add(doremir_map_t,
                              doremir_map_key_t,
                              doremir_map_value_t);
void doremir_map_remove(doremir_map_t,
                        doremir_map_key_t,
                        doremir_map_value_t);
doremir_map_t doremir_map_add_dest(doremir_map_t,
                                   doremir_map_key_t,
                                   doremir_map_value_t);
void doremir_map_remove_dest(doremir_map_t,
                             doremir_map_key_t,
                             doremir_map_value_t);

/** @}
    @}
    */

#endif // _DOREMIR_MAP

