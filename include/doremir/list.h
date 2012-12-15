
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
doremir_list_t doremir_list_empty();
doremir_list_t doremir_list_cons(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_snoc(doremir_ptr_t, doremir_list_t);
void doremir_list_destroy(doremir_list_t);
doremir_list_t doremir_list_copy(doremir_list_t);
void doremir_list_swap(doremir_list_t, doremir_list_t);
bool doremir_list_is_empty(doremir_list_t);
int doremir_list_lenght(doremir_list_t);
doremir_list_t doremir_list_take(int, doremir_list_t);
doremir_list_t doremir_list_drop(int, doremir_list_t);
bool doremir_list_is_elem(doremir_ptr_t, doremir_list_t);
doremir_list_t doremir_list_reverse(doremir_list_t);
doremir_list_t doremir_list_sort(doremir_list_t);
doremir_list_t doremir_list_filter(doremir_pred_t, doremir_list_t);
doremir_list_t doremir_list_map(doremir_list_t, doremir_unary_t);
doremir_list_t doremir_list_foldl(doremir_list_t,
                                  doremir_binary_t,
                                  doremir_ptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_LIST

