
#ifndef _DOREMIR_LIST
#define _DOREMIR_LIST

#include <doremir.h>
#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirList List
    @{
    */

typedef struct _doremir_list_t * doremir_list_t;
typedef doremir_list_t doremir_list_ord_list_t;
typedef doremir_list_t doremir_list_list_list_t;
doremir_list_t doremir_list_empty();
doremir_list_t doremir_list_single(doremir_ptr_t);
doremir_list_t doremir_list_cons(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_snoc(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_append(doremir_list_t, doremir_list_t);
doremir_list_t doremir_list_copy(doremir_list_t);
void doremir_list_destroy(doremir_list_t);
bool doremir_list_is_empty(doremir_list_t);
int doremir_list_lenght(doremir_list_t);
doremir_ptr_t doremir_list_head(doremir_list_t);
doremir_ptr_t doremir_list_tail(doremir_list_t);
doremir_ptr_t doremir_list_init(doremir_list_t);
doremir_ptr_t doremir_list_last(doremir_list_t);
doremir_list_t doremir_list_take(int, doremir_list_t);
doremir_list_t doremir_list_drop(int, doremir_list_t);
bool doremir_list_is_elem(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_reverse(doremir_list_t);
doremir_list_t doremir_list_sort(doremir_list_ord_list_t);
doremir_ptr_t doremir_list_find(doremir_pred_t, doremir_list_t);
doremir_list_t doremir_list_filter(doremir_pred_t, doremir_list_t);
bool doremir_list_any(doremir_pred_t, doremir_list_t);
bool doremir_list_all(doremir_pred_t, doremir_list_t);
doremir_list_t doremir_list_map(doremir_unary_t, doremir_list_t);
doremir_list_t doremir_list_fold(doremir_binary_t,
                                 doremir_ptr_t,
                                 doremir_list_t);
doremir_list_t doremir_list_concat(doremir_list_list_list_t);
doremir_ptr_t doremir_list_sum(doremir_list_t);
doremir_ptr_t doremir_list_product(doremir_list_t);
doremir_ptr_t doremir_list_maximum(doremir_list_t);
doremir_ptr_t doremir_list_minimum(doremir_list_t);
doremir_list_t doremir_list_consd(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_snocd(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_reversed(doremir_list_t);
doremir_list_t doremir_list_sortd(doremir_list_t);
doremir_list_t doremir_list_mapd(doremir_unary_t, doremir_list_t);
doremir_list_t doremir_list_foldd(doremir_binary_t,
                                  doremir_ptr_t,
                                  doremir_list_t);

/** @}
    @}
    */

#endif // _DOREMIR_LIST

