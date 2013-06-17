
#ifndef _FAE_SET
#define _FAE_SET

#include <fae.h>
#include <fae/list.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeSet Set
    @{
    */

typedef struct _fae_set_t * fae_set_t;
fae_set_t fae_set_empty();
fae_set_t fae_set_single(fae_ptr_t);
fae_set_t fae_set_add(fae_ptr_t, fae_set_t);
fae_set_t fae_set_set(fae_ptr_t, fae_set_t);
fae_set_t fae_set_remove(fae_ptr_t, fae_set_t);
fae_set_t fae_set_dadd(fae_ptr_t, fae_set_t);
fae_set_t fae_set_dset(fae_ptr_t, fae_set_t);
fae_set_t fae_set_dremove(fae_ptr_t, fae_set_t);
fae_set_t fae_set_copy(fae_set_t);
void fae_set_destroy(fae_set_t);
int fae_set_size(fae_set_t);
bool fae_set_is_empty(fae_set_t);
bool fae_set_is_single(fae_set_t);
bool fae_set_has(fae_ptr_t, fae_set_t);
fae_ptr_t fae_set_get(fae_ptr_t, fae_set_t);
bool fae_set_is_subset_of(fae_set_t, fae_set_t);
bool fae_set_is_proper_subset_of(fae_set_t, fae_set_t);
fae_set_t fae_set_sum(fae_set_t, fae_set_t);
fae_set_t fae_set_intersection(fae_set_t, fae_set_t);
fae_set_t fae_set_difference(fae_set_t, fae_set_t);
fae_set_t fae_set_product(fae_set_t, fae_set_t);
fae_set_t fae_set_power(fae_set_t);
fae_set_t fae_set_from_list(fae_list_t);
fae_list_t fae_set_to_list(fae_set_t);

/** @}
    @}
    */

#endif // _FAE_SET

