
#ifndef _FA_LIST
#define _FA_LIST

#include <fa.h>

/** @addtogroup FaList

    @addtogroup FaList

    Immutable list data structure.

    - Fast front access: ([empty](@ref fa_list_empty), 
                          [single](@ref fa_list_single), 
                          [cons](@ref fa_list_cons), 
                          [head](@ref fa_list_head), 
                          [tail](@ref fa_list_tail)).

    - Slow back access ([init](@ref fa_list_init), 
                        [last](@ref fa_list_last)).

    - Slow random access ([index](@ref fa_list_index), 
                          [insert](@ref fa_list_insert), 
                          [remove](@ref fa_list_remove)).

    - Slow range access: ([take](@ref fa_list_take), 
                          [drop](@ref fa_list_drop), 
                          [range](@ref fa_list_range), 
                          [insertRange](@ref fa_list_insert_range), 
                          [removeRange](@ref fa_list_remove_range))

    - Linear [find](@ref fa_list_find), 
             [map](@ref fa_list_map), 
             [fold](@ref fa_list_fold), 
             [reverse](@ref fa_list_reverse) and 
             [filter](@ref fa_list_filter).

    - Logarithmic [sort](@ref fa_list_sort).

    @par Literals
    - `list(1, 2, 3)`

    @par Display
    - `[1,2,3]`
    
    @par Iteration
    ~~~
    fa_list_for_each (list(1,2,3), x)
    {
        fa_print("%s\n", x);
    }
    ~~~

    @par Implements
    - fa_equal_t
    - fa_order_t
    - fa_copy_t
    - fa_destroy_t
    - fa_dynamic_t
    - fa_string_show_t

    @see
    - [Data structures](@ref DataStructures)

 
    @defgroup Fa Fa
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

