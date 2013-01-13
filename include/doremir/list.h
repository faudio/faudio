
#ifndef _DOREMIR_LIST
#define _DOREMIR_LIST

#include <doremir.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirList List
    @{
    */

typedef struct _doremir_list_t * doremir_list_t;
doremir_list_t doremir_list_empty();
doremir_list_t doremir_list_single(doremir_ptr_t);
doremir_list_t doremir_list_cons(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_dcons(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_repeat(int, doremir_ptr_t);
doremir_list_t doremir_list_enum_from(int, int);
doremir_list_t doremir_list_copy(doremir_list_t);
void doremir_list_destroy(doremir_list_t);
bool doremir_list_is_empty(doremir_list_t);
bool doremir_list_is_single(doremir_list_t);
int doremir_list_length(doremir_list_t);
doremir_ptr_t doremir_list_head(doremir_list_t);
doremir_list_t doremir_list_tail(doremir_list_t);
doremir_list_t doremir_list_dtail(doremir_list_t);
doremir_list_t doremir_list_init(doremir_list_t);
doremir_list_t doremir_list_dinit(doremir_list_t);
doremir_ptr_t doremir_list_last(doremir_list_t);
doremir_list_t doremir_list_append(doremir_list_t, doremir_list_t);
doremir_list_t doremir_list_dappend(doremir_list_t,
                                    doremir_list_t);
doremir_list_t doremir_list_reverse(doremir_list_t);
doremir_list_t doremir_list_dreverse(doremir_list_t);
doremir_list_t doremir_list_sort(doremir_list_t);
doremir_list_t doremir_list_dsort(doremir_list_t);
doremir_list_t doremir_list_take(int, doremir_list_t);
doremir_list_t doremir_list_dtake(int, doremir_list_t);
doremir_list_t doremir_list_drop(int, doremir_list_t);
doremir_list_t doremir_list_ddrop(int, doremir_list_t);
doremir_ptr_t doremir_list_index(int, doremir_list_t);
doremir_list_t doremir_list_range(int, int, doremir_list_t);
doremir_list_t doremir_list_insert(int,
                                   doremir_ptr_t,
                                   doremir_list_t);
doremir_list_t doremir_list_dinsert(int,
                                    doremir_ptr_t,
                                    doremir_list_t);
doremir_list_t doremir_list_insert_range(int,
                                         doremir_list_t,
                                         doremir_list_t);
doremir_list_t doremir_list_dinsert_range(int,
                                          doremir_list_t,
                                          doremir_list_t);
doremir_list_t doremir_list_remove(int, doremir_list_t);
doremir_list_t doremir_list_dremove(int, doremir_list_t);
doremir_list_t doremir_list_remove_range(int, int, doremir_list_t);
doremir_list_t doremir_list_dremove_range(int,
                                          int,
                                          doremir_list_t);
bool doremir_list_has(doremir_ptr_t, doremir_list_t);
doremir_ptr_t doremir_list_find(doremir_pred_t, doremir_list_t);
int doremir_list_index_of(doremir_ptr_t, doremir_list_t);
int doremir_list_find_index(doremir_pred_t, doremir_list_t);
doremir_list_t doremir_list_filter(doremir_pred_t,
                                   doremir_ptr_t,
                                   doremir_list_t);
doremir_list_t doremir_list_dfilter(doremir_pred_t,
                                    doremir_ptr_t,
                                    doremir_list_t);
doremir_list_t doremir_list_map(doremir_unary_t,
                                doremir_ptr_t,
                                doremir_list_t);
doremir_list_t doremir_list_dmap(doremir_unary_t,
                                 doremir_ptr_t,
                                 doremir_list_t);
doremir_list_t doremir_list_concat_map(doremir_unary_t,
                                       doremir_ptr_t,
                                       doremir_list_t);
doremir_list_t doremir_list_dconcat_map(doremir_unary_t,
                                        doremir_ptr_t,
                                        doremir_list_t);
doremir_ptr_t doremir_list_fold_left(doremir_binary_t,
                                     doremir_ptr_t,
                                     doremir_ptr_t,
                                     doremir_list_t);
doremir_ptr_t doremir_list_dfold_left(doremir_binary_t,
                                      doremir_ptr_t,
                                      doremir_ptr_t,
                                      doremir_list_t);
doremir_list_t doremir_list_concat(doremir_list_t);
doremir_list_t doremir_list_dconcat(doremir_list_t);

/** @}
    @}
    */

#endif // _DOREMIR_LIST

