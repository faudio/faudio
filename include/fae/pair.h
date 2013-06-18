
#ifndef _FAE_PAIR
#define _FAE_PAIR

#include <fae.h>
#include <fae/list.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaePair Pair
    @{
    */

typedef struct _fae_pair_t * fae_pair_t;
fae_pair_t fae_pair_create(fae_ptr_t, fae_ptr_t);
fae_pair_t fae_pair_copy(fae_pair_t);
void fae_pair_destroy(fae_pair_t);
fae_ptr_t fae_pair_first(fae_pair_t);
fae_ptr_t fae_pair_second(fae_pair_t);
fae_pair_t fae_pair_duplicate(fae_ptr_t);
fae_pair_t fae_pair_swap(fae_pair_t);
fae_pair_t fae_pair_assoc(fae_pair_t);
fae_pair_t fae_pair_unassoc(fae_pair_t);
fae_list_t fae_pair_to_list(fae_pair_t);

/** @}
    @}
    */

#endif // _FAE_PAIR

