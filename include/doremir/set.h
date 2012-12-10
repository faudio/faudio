
#ifndef _DOREMIR_SET
#define _DOREMIR_SET

#include <doremir/std.h>

/** @defgroup Doremir
    @{
    @defgroup Set
    @{
    */

typedef struct _doremir_set_t * doremir_set_t;
typedef intptr_t doremir_set_value_t;
bool doremir_set_elem(doremir_set_t, doremir_set_value_t);
bool doremir_set_subset_of(doremir_set_t, doremir_set_t);
bool doremir_set_proper_subset_of(doremir_set_t, doremir_set_t);
doremir_set_t doremir_set_empty();
doremir_set_t doremir_set_add(doremir_set_t, doremir_set_value_t);
void doremir_set_remove(doremir_set_t, doremir_set_value_t);
doremir_set_t doremir_set_add_unique(doremir_set_t,
                                     doremir_set_value_t);
void doremir_set_remove_unique(doremir_set_t, doremir_set_value_t);
doremir_set_t doremir_set_union_of(doremir_set_t, doremir_set_t);
doremir_set_t doremir_set_product_of(doremir_set_t, doremir_set_t);
doremir_set_t doremir_set_symmetric_difference_of(doremir_set_t,
                                                  doremir_set_t);
doremir_set_t doremir_set_cartesian_product_of(doremir_set_t,
                                               doremir_set_t);
doremir_set_t doremir_set_power_set_of(doremir_set_t);

/** @}
    @}
    */

#endif // _DOREMIR_SET

