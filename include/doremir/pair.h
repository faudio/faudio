
#ifndef _DOREMIR_PAIR
#define _DOREMIR_PAIR

#include <doremir.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPair Pair
    @{
    */

typedef struct _doremir_pair_t * doremir_pair_t;
doremir_pair_t doremir_pair_create(doremir_ptr_t, doremir_ptr_t);
void doremir_pair_destroy(doremir_pair_t);
doremir_ptr_t doremir_pair_fst(doremir_pair_t);
doremir_ptr_t doremir_pair_snd(doremir_pair_t);
doremir_pair_t doremir_pair_dup(doremir_ptr_t);
doremir_pair_t doremir_pair_swap(doremir_pair_t);
doremir_pair_t doremir_pair_assoc(doremir_pair_t);
doremir_pair_t doremir_pair_map(doremir_unary_t, doremir_pair_t);
bool doremir_pair_equal(doremir_pair_t, doremir_pair_t);
bool doremir_pair_less_than(doremir_pair_t, doremir_pair_t);
bool doremir_pair_greater_than(doremir_pair_t, doremir_pair_t);
doremir_string_t doremir_pair_show(doremir_pair_t);

/** @}
    @}
    */

#endif // _DOREMIR_PAIR

