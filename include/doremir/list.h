
#ifndef _DOREMIR_LIST
#define _DOREMIR_LIST

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirList List
    @{
    */

typedef struct _doremir_list_t * doremir_list_t;
typedef intptr_t doremir_list_value_t;
doremir_list_t doremir_list_empty();
doremir_list_t doremir_list_cons(doremir_list_value_t,
                                 doremir_list_t);
doremir_list_t doremir_list_snoc(doremir_list_value_t,
                                 doremir_list_t);
void doremir_list_destroy(doremir_list_t);
doremir_list_t doremir_list_copy(doremir_list_t);
void doremir_list_swap(doremir_list_t, doremir_list_t);
bool doremir_list_is_empty(doremir_list_t);
int doremir_list_lenght(doremir_list_t);
doremir_list_t doremir_list_take(int, doremir_list_t);
doremir_list_t doremir_list_drop(int, doremir_list_t);
bool doremir_list_is_elem(doremir_list_value_t, doremir_list_t);
doremir_list_t doremir_list_reverse(doremir_list_t);
doremir_list_t doremir_list_sort(doremir_list_t);
doremir_list_t doremir_list_sep(doremir_list_value_t,
                                doremir_list_t);
doremir_list_t doremir_list_concat(doremir_list_t);
doremir_list_t doremir_list_concat_sep(doremir_list_t);
typedef bool (* doremir_list_pred_t)(doremir_list_value_t);
doremir_list_t doremir_list_filter(doremir_list_pred_t,
                                   doremir_list_t);
bool doremir_list_and(doremir_list_t);
bool doremir_list_or(doremir_list_t);
bool doremir_list_any(doremir_list_pred_t, doremir_list_t);
bool doremir_list_all(doremir_list_pred_t, doremir_list_t);
typedef doremir_list_value_t (* doremir_list_unary_t)(doremir_list_value_t);
typedef doremir_list_value_t (* doremir_list_binary_t)(doremir_list_value_t,
                                                       doremir_list_value_t);
doremir_list_t doremir_list_map(doremir_list_t,
                                doremir_list_unary_t);
doremir_list_t doremir_list_foldl(doremir_list_t,
                                  doremir_list_binary_t,
                                  doremir_list_value_t);
typedef struct {
            doremir_list_value_t (* add)(doremir_list_value_t,
                                         doremir_list_value_t);
            doremir_list_value_t (* sub)(doremir_list_value_t,
                                         doremir_list_value_t);
            doremir_list_value_t (* mul)(doremir_list_value_t,
                                         doremir_list_value_t);
            doremir_list_value_t (* div)(doremir_list_value_t,
                                         doremir_list_value_t);
            doremir_list_value_t (* mod)(doremir_list_value_t,
                                         doremir_list_value_t);
            doremir_list_value_t (* abs)(doremir_list_value_t,
                                         doremir_list_value_t);
        } doremir_list_num_t;
doremir_list_value_t doremir_list_maximum(doremir_list_t);
doremir_list_value_t doremir_list_minimum(doremir_list_t);

/** @}
    @}
    */

#endif // _DOREMIR_LIST

