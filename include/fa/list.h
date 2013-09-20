
#ifndef _FA_LIST
#define _FA_LIST

#include <fa.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaList List
    @{
    */

typedef struct _fa_list_t * fa_list_t;
fa_list_t fa_list_empty();
fa_list_t fa_list_single(fa_ptr_t);
fa_list_t fa_list_cons(fa_ptr_t, fa_list_t);
fa_list_t fa_list_dcons(fa_ptr_t, fa_list_t);
fa_list_t fa_list_repeat(int, fa_ptr_t);
fa_list_t fa_list_enumerate(int, int);
fa_list_t fa_list_copy(fa_list_t);
void fa_list_destroy(fa_list_t);
bool fa_list_is_empty(fa_list_t);
bool fa_list_is_single(fa_list_t);
int fa_list_length(fa_list_t);
fa_ptr_t fa_list_head(fa_list_t);
fa_list_t fa_list_tail(fa_list_t);
fa_list_t fa_list_dtail(fa_list_t);
fa_list_t fa_list_init(fa_list_t);
fa_list_t fa_list_dinit(fa_list_t);
fa_ptr_t fa_list_last(fa_list_t);
fa_list_t fa_list_append(fa_list_t, fa_list_t);
fa_list_t fa_list_dappend(fa_list_t, fa_list_t);
fa_list_t fa_list_reverse(fa_list_t);
fa_list_t fa_list_dreverse(fa_list_t);
fa_list_t fa_list_sort(fa_list_t);
fa_list_t fa_list_dsort(fa_list_t);
fa_list_t fa_list_take(int, fa_list_t);
fa_list_t fa_list_dtake(int, fa_list_t);
fa_list_t fa_list_drop(int, fa_list_t);
fa_list_t fa_list_ddrop(int, fa_list_t);
fa_ptr_t fa_list_index(int, fa_list_t);
fa_list_t fa_list_range(int, int, fa_list_t);
fa_list_t fa_list_insert(int, fa_ptr_t, fa_list_t);
fa_list_t fa_list_dinsert(int, fa_ptr_t, fa_list_t);
fa_list_t fa_list_insert_range(int, fa_list_t, fa_list_t);
fa_list_t fa_list_dinsert_range(int, fa_list_t, fa_list_t);
fa_list_t fa_list_remove(int, fa_list_t);
fa_list_t fa_list_dremove(int, fa_list_t);
fa_list_t fa_list_remove_range(int, int, fa_list_t);
fa_list_t fa_list_dremove_range(int, int, fa_list_t);
bool fa_list_has(fa_ptr_t, fa_list_t);
fa_ptr_t fa_list_find(fa_pred_t, fa_ptr_t, fa_list_t);
int fa_list_index_of(fa_ptr_t, fa_list_t);
int fa_list_find_index(fa_pred_t, fa_ptr_t, fa_list_t);
fa_list_t fa_list_filter(fa_pred_t, fa_ptr_t, fa_list_t);
fa_list_t fa_list_dfilter(fa_pred_t, fa_ptr_t, fa_list_t);
fa_list_t fa_list_map(fa_unary_t, fa_ptr_t, fa_list_t);
fa_list_t fa_list_dmap(fa_unary_t, fa_ptr_t, fa_list_t);
fa_list_t fa_list_join_map(fa_unary_t, fa_ptr_t, fa_list_t);
fa_list_t fa_list_djoin_map(fa_unary_t, fa_ptr_t, fa_list_t);
fa_list_t fa_list_join(fa_list_t);
fa_list_t fa_list_djoin(fa_list_t);
fa_ptr_t fa_list_fold_left(fa_binary_t,
                           fa_ptr_t,
                           fa_ptr_t,
                           fa_list_t);
fa_ptr_t fa_list_dfold_left(fa_binary_t,
                            fa_ptr_t,
                            fa_ptr_t,
                            fa_list_t);
fa_list_t fa_list_to_list(fa_list_t);

/** @}
    @}
    */

#endif // _FA_LIST

