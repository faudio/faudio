
#ifndef _FAE_LIST
#define _FAE_LIST

#include <fae.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeList List
    @{
    */

typedef struct _fae_list_t * fae_list_t;
fae_list_t fae_list_empty();
fae_list_t fae_list_single(fae_ptr_t);
fae_list_t fae_list_cons(fae_ptr_t, fae_list_t);
fae_list_t fae_list_dcons(fae_ptr_t, fae_list_t);
fae_list_t fae_list_repeat(int, fae_ptr_t);
fae_list_t fae_list_enumerate(int, int);
fae_list_t fae_list_copy(fae_list_t);
void fae_list_destroy(fae_list_t);
bool fae_list_is_empty(fae_list_t);
bool fae_list_is_single(fae_list_t);
int fae_list_length(fae_list_t);
fae_ptr_t fae_list_head(fae_list_t);
fae_list_t fae_list_tail(fae_list_t);
fae_list_t fae_list_dtail(fae_list_t);
fae_list_t fae_list_init(fae_list_t);
fae_list_t fae_list_dinit(fae_list_t);
fae_ptr_t fae_list_last(fae_list_t);
fae_list_t fae_list_append(fae_list_t, fae_list_t);
fae_list_t fae_list_dappend(fae_list_t, fae_list_t);
fae_list_t fae_list_reverse(fae_list_t);
fae_list_t fae_list_dreverse(fae_list_t);
fae_list_t fae_list_sort(fae_list_t);
fae_list_t fae_list_dsort(fae_list_t);
fae_list_t fae_list_take(int, fae_list_t);
fae_list_t fae_list_dtake(int, fae_list_t);
fae_list_t fae_list_drop(int, fae_list_t);
fae_list_t fae_list_ddrop(int, fae_list_t);
fae_ptr_t fae_list_index(int, fae_list_t);
fae_list_t fae_list_range(int, int, fae_list_t);
fae_list_t fae_list_insert(int, fae_ptr_t, fae_list_t);
fae_list_t fae_list_dinsert(int, fae_ptr_t, fae_list_t);
fae_list_t fae_list_insert_range(int, fae_list_t, fae_list_t);
fae_list_t fae_list_dinsert_range(int, fae_list_t, fae_list_t);
fae_list_t fae_list_remove(int, fae_list_t);
fae_list_t fae_list_dremove(int, fae_list_t);
fae_list_t fae_list_remove_range(int, int, fae_list_t);
fae_list_t fae_list_dremove_range(int, int, fae_list_t);
bool fae_list_has(fae_ptr_t, fae_list_t);
fae_ptr_t fae_list_find(fae_pred_t, fae_ptr_t, fae_list_t);
int fae_list_index_of(fae_ptr_t, fae_list_t);
int fae_list_find_index(fae_pred_t, fae_ptr_t, fae_list_t);
fae_list_t fae_list_filter(fae_pred_t, fae_ptr_t, fae_list_t);
fae_list_t fae_list_dfilter(fae_pred_t, fae_ptr_t, fae_list_t);
fae_list_t fae_list_map(fae_unary_t, fae_ptr_t, fae_list_t);
fae_list_t fae_list_dmap(fae_unary_t, fae_ptr_t, fae_list_t);
fae_list_t fae_list_join_map(fae_unary_t, fae_ptr_t, fae_list_t);
fae_list_t fae_list_djoin_map(fae_unary_t, fae_ptr_t, fae_list_t);
fae_list_t fae_list_join(fae_list_t);
fae_list_t fae_list_djoin(fae_list_t);
fae_ptr_t fae_list_fold_left(fae_binary_t,
                             fae_ptr_t,
                             fae_ptr_t,
                             fae_list_t);
fae_ptr_t fae_list_dfold_left(fae_binary_t,
                              fae_ptr_t,
                              fae_ptr_t,
                              fae_list_t);
fae_list_t fae_list_to_list(fae_list_t);

/** @}
    @}
    */

#endif // _FAE_LIST

