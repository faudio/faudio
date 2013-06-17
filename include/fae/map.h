
#ifndef _FAE_MAP
#define _FAE_MAP

#include <fae.h>
#include <fae/pair.h>
#include <fae/list.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeMap Map
    @{
    */

typedef struct _fae_map_t * fae_map_t;
typedef fae_ptr_t fae_map_key_t;
fae_map_t fae_map_empty();
fae_map_t fae_map_copy(fae_map_t);
void fae_map_destroy(fae_map_t);
int fae_map_size(fae_map_t);
bool fae_map_is_empty(fae_map_t);
bool fae_map_is_single(fae_map_t);
fae_map_t fae_map_add(fae_map_key_t, fae_ptr_t, fae_map_t);
fae_map_t fae_map_set(fae_map_key_t, fae_ptr_t, fae_map_t);
fae_map_t fae_map_remove(fae_map_key_t, fae_map_t);
fae_map_t fae_map_dadd(fae_map_key_t, fae_ptr_t, fae_map_t);
fae_map_t fae_map_dset(fae_map_key_t, fae_ptr_t, fae_map_t);
fae_map_t fae_map_dremove(fae_map_key_t, fae_map_t);
fae_map_t fae_map_add_entry(fae_pair_t, fae_map_t);
fae_map_t fae_map_set_entry(fae_pair_t, fae_map_t);
fae_map_t fae_map_remove_entry(fae_pair_t, fae_map_t);
fae_ptr_t fae_map_get(fae_map_key_t, fae_map_t);
bool fae_map_has_key(fae_map_key_t, fae_map_t);
bool fae_map_has_elem(fae_ptr_t, fae_map_t);
bool fae_map_has_entry(fae_pair_t, fae_map_t);
bool fae_map_is_submap_of(fae_map_t, fae_map_t);
bool fae_map_is_proper_submap_of(fae_map_t, fae_map_t);
fae_map_t fae_map_sum(fae_map_t, fae_map_t);
fae_map_t fae_map_product(fae_map_t, fae_map_t);
fae_map_t fae_map_difference(fae_map_t, fae_map_t);
fae_map_t fae_map_power(fae_map_t);
fae_map_t fae_map_map(fae_unary_t, fae_ptr_t, fae_map_t);
fae_map_t fae_map_from_pair(fae_pair_t);
fae_map_t fae_map_from_list(fae_list_t);
fae_list_t fae_map_to_list(fae_map_t);

/** @}
    @}
    */

#endif // _FAE_MAP

