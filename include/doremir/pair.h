
#ifndef _DOREMIR_PAIR
#define _DOREMIR_PAIR

#include <doremir.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPair Pair
    @{
    */

typedef struct _doremir_pair_t *doremir_pair_t;
doremir_pair_t doremir_pair_create(doremir_ptr_t, doremir_ptr_t);
doremir_pair_t doremir_pair_copy(doremir_pair_t);
void doremir_pair_destroy(doremir_pair_t);
doremir_ptr_t doremir_pair_fst(doremir_pair_t);
doremir_ptr_t doremir_pair_snd(doremir_pair_t);
doremir_pair_t doremir_pair_dup(doremir_ptr_t);
doremir_pair_t doremir_pair_swap(doremir_pair_t);
doremir_pair_t doremir_pair_assoc(doremir_pair_t);
doremir_pair_t doremir_pair_unassoc(doremir_pair_t);

/** @}
    @}
    */

#endif // _DOREMIR_PAIR

