
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

/** Create an empty list.

    The returned list must be destroyed by passing to a destructive function.

    @return
        A new list.
    @par Performance
        O(1)
*/
fa_list_t fa_list_empty();

/** Create a new list containing the given element.

    The returned list must be destroyed by passing to a destructive function.

    @param value
        Value to store.
    @return
        A new list.
    @par Performance
        O(1)
*/
fa_list_t fa_list_single(fa_ptr_t);

/** Create a new list by inserting the given element at the beginning of the given list.

    The returned list must be destroyed by passing to a destructive function.

    @param head
        Value to form head of list.
    @param tail
        List to form tail of list.
    @return
        A new list.
    @par Performance
        O(1)
*/
fa_list_t fa_list_cons(fa_ptr_t, fa_list_t);


fa_list_t fa_list_dcons(fa_ptr_t, fa_list_t);

/** Create a list by repeating the given element.
*/
fa_list_t fa_list_repeat(int, fa_ptr_t);

/** Create a list from the given range.
*/
fa_list_t fa_list_enumerate(int, int);

/** Copy the given list.

    The returned list must be destroyed by passing to a destructive function.

    @par Performance
        O(1)
*/
fa_list_t fa_list_copy(fa_list_t);

/** Destroy the given list.

    @par Performance
        O(n)
*/
void fa_list_destroy(fa_list_t);

/** Return whether the given list is empty.
    @par Performance
        O(1)
*/
bool fa_list_is_empty(fa_list_t);

/** Return whether the given list has a single element.
    @par Performance
        O(1)
*/
bool fa_list_is_single(fa_list_t);

/** Return the lenght of the given list.
    @par Performance
        O(n)
*/
int fa_list_length(fa_list_t);

/** Return the first element of the given list.
    @par Performance
        O(1)
*/
fa_ptr_t fa_list_head(fa_list_t);

/** Return all elements but the first of the given list.
    @par Performance
        O(1)
*/
fa_list_t fa_list_tail(fa_list_t);


fa_list_t fa_list_dtail(fa_list_t);

/** Return all elements but the last of the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_init(fa_list_t);


fa_list_t fa_list_dinit(fa_list_t);

/** Return the last element of the given list.
    @par Performance
        O(n)
*/
fa_ptr_t fa_list_last(fa_list_t);

/** Return the result of appending the given lists.
    @par Performance
        O(n)
*/
fa_list_t fa_list_append(fa_list_t, fa_list_t);


fa_list_t fa_list_dappend(fa_list_t, fa_list_t);

/** Return the reverse of the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_reverse(fa_list_t);


fa_list_t fa_list_dreverse(fa_list_t);

/** Return the given list sorted.
    @par Performance
        O(n log n)
*/
fa_list_t fa_list_sort(fa_list_t);


fa_list_t fa_list_dsort(fa_list_t);

/** Return the *n* leading elements of the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_take(int, fa_list_t);


fa_list_t fa_list_dtake(int, fa_list_t);

/** Return the all but the *n* leading elements of the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_drop(int, fa_list_t);


fa_list_t fa_list_ddrop(int, fa_list_t);

/** List index operator.
    @returns
        The nth element of the given list.
    @par Performance
        O(n)
*/
fa_ptr_t fa_list_index(int, fa_list_t);

/** Return the given range of the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_range(int, int, fa_list_t);

/** Insert the given element into the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_insert(int, fa_ptr_t, fa_list_t);


fa_list_t fa_list_dinsert(int, fa_ptr_t, fa_list_t);

/** Insert the given range into the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_insert_range(int, fa_list_t, fa_list_t);


fa_list_t fa_list_dinsert_range(int, fa_list_t, fa_list_t);

/** Remove the given element from the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_remove(int, fa_list_t);


fa_list_t fa_list_dremove(int, fa_list_t);

/** Remove the given range from the given list.
    @par Performance
        O(n)
*/
fa_list_t fa_list_remove_range(int, int, fa_list_t);


fa_list_t fa_list_dremove_range(int, int, fa_list_t);

/** Return whether the given list contains the given element.
    @par Performance
        O(n)
*/
bool fa_list_has(fa_ptr_t, fa_list_t);

/** Return the first element satisfying the given predicate in the
    list, if found.
    @param list     List.
    @param value    Value to search for.
    @return         Index of the found value (optional).
    @par Performance
        O(log n)
*/
fa_ptr_t fa_list_find(fa_pred_t, fa_ptr_t, fa_list_t);

/** Return the index of the first occurance given element in the
    list, or a negative value if no such element is found
    @par Performance
        O(n)
*/
int fa_list_index_of(fa_ptr_t, fa_list_t);

/** Return the index of the first element satisfying the given predicate in the
    list, or a negative value if no such element is found.
    @par Performance
        O(log n)
*/
int fa_list_find_index(fa_pred_t, fa_ptr_t, fa_list_t);

/** Return the given list with all elements not satisfying the given predicate removed.
    @par Performance
        O(n)
*/
fa_list_t fa_list_filter(fa_pred_t, fa_ptr_t, fa_list_t);


fa_list_t fa_list_dfilter(fa_pred_t, fa_ptr_t, fa_list_t);

/** Return the result of applying the given function to all elements of the given list.

    @par Laws

        map(apply1, id, xs)                == xs
        map(apply1, f, map(apply1, g, xs)) == map(apply1, comp(f, g), xs)

    @par Performance
        O(n)
*/
fa_list_t fa_list_map(fa_unary_t, fa_ptr_t, fa_list_t);


fa_list_t fa_list_dmap(fa_unary_t, fa_ptr_t, fa_list_t);

/** Map over the given list and join the results.

    This function is useful to apply functions from singletons to lists.

    @par Laws

        joinMap(apply1, single, xs) == xs`

    @par Performance
        O(n)
*/
fa_list_t fa_list_join_map(fa_unary_t, fa_ptr_t, fa_list_t);


fa_list_t fa_list_djoin_map(fa_unary_t, fa_ptr_t, fa_list_t);

/** Concatenate all elements of the given list.

    The given list must contain lists only.

    @par Performance
        O(n)
*/
fa_list_t fa_list_join(fa_list_t);


fa_list_t fa_list_djoin(fa_list_t);

/** Fold over the given list from left to right.

    @par Performance
        O(n)
*/
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

