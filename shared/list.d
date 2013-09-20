
/*
    faudio
    
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

/** Create an empty list.

    The returned list must be destroyed by passing to a destructive function.

    @return
        A new list.
    @par Performance
        O(1)
 */
list_t fa_list_empty() {}

/** Create a new list containing the given element.

    The returned list must be destroyed by passing to a destructive function.

    @param value
        Value to store.
    @return
        A new list.
    @par Performance
        O(1)
 */
list_t fa_list_single(fa_ptr_t value) {}

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
list_t fa_list_cons(fa_ptr_t head, fa_list_t tail) {}

/** Create a new list by inserting the given element at the beginning of the given list.

    The returned list must be destroyed by passing to a destructive function.

    @param head
        Value to form head of list.
    @param tail
        List to form tail of list (destroyed).
    @return
        A new list.
    @par Performance
        O(1)
 */
list_t fa_list_dcons(fa_ptr_t head, fa_list_t tail) {}

/** Copy the given list.

    The returned list must be destroyed by passing to a destructive function.

    @par Performance
        O(1)
 */
list_t fa_list_copy(fa_list_t list) {}

/** Destroy the given list.

    @par Performance
        O(n)
 */
void fa_list_destroy(fa_list_t list) {}

// --------------------------------------------------------------------------------

/** Return whether the given list is empty.
    @par Performance
        O(1)
 */
bool fa_list_is_empty(fa_list_t list) {}

/** Return whether the given list has a single element.
    @par Performance
        O(1)
 */
bool fa_list_is_single(fa_list_t list) {}

/** Return the lenght of the given list.
    @par Performance
        O(n)
 */
int fa_list_length(fa_list_t list) {}


// --------------------------------------------------------------------------------
// Sequential access
// --------------------------------------------------------------------------------

/** Return the first element of the given list.
    @par Performance
        O(1)
 */
fa_ptr_t fa_list_head(fa_list_t list) {}

/** Return all elements but the first of the given list.
    @par Performance
        O(1)
 */
fa_list_t fa_list_tail(fa_list_t list) {}

/** Return all elements but the last of the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_init(fa_list_t list) {}

/** Return all elements but the first of the given list.
    @par Performance
        O(1)
 */
fa_list_t fa_list_dtail(fa_list_t list) {}

/** Return all elements but the last of the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_dinit(fa_list_t list) {}

/** Return the last element of the given list.
    @par Performance
        O(n)
 */
fa_ptr_t fa_list_last(fa_list_t list) {}


// --------------------------------------------------------------------------------
// Misc operations
// --------------------------------------------------------------------------------

/** Return the result of appending the given lists.
    @par Performance
        O(n)
 */
list_t fa_list_append(fa_list_t list, fa_list_t list2) {}

/** Return the result of appending the given lists.

    This function destroys both of the given lists.
    @par Performance
        O(n)
 */
list_t fa_list_dappend(fa_list_t list, fa_list_t list2) {}

/** Return the reverse of the given list.
    @par Performance
        O(n)
 */
list_t fa_list_reverse(fa_list_t list) {}

/** Return the reverse of the given list.

    This function destroys the given list.
    @par Performance
        O(n)
 */
list_t fa_list_dreverse(fa_list_t list) {}

/** Return the given list sorted.
    @par Performance
        O(n log n)
 */
list_t fa_list_sort(fa_list_t list) {}

/** Return the given list sorted.

    This function destroys the given list.
    @par Performance
        O(n log n)
 */
list_t fa_list_dsort(fa_list_t list) {}


// --------------------------------------------------------------------------------
// Random access
// --------------------------------------------------------------------------------

/** Return the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fa_list_take(int end, fa_list_t list) {}

/** Return the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fa_list_dtake(int end, fa_list_t list) {}

/** Return the all but the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fa_list_drop(int end, fa_list_t list) {}

/** Return the all but the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fa_list_ddrop(int end, fa_list_t list) {}

/** List index operator.
    @returns
        The nth element of the given list.
    @par Performance
        O(n)
 */
fa_ptr_t fa_list_index(int end, fa_list_t list) {}

/** Return the given range of the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_range(int begin, int end, fa_list_t list) {}

/** Insert the given element into the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_insert(int index, fa_ptr_t value, fa_list_t list) {}

/** Insert the given element into the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_dinsert(int begin, fa_ptr_t x, fa_list_t list) {}

/** Insert the given range into the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_insert_range(int begin, fa_list_t list, fa_list_t list2) {}

/** Insert the given range into the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_dinsert_range(int begin, fa_list_t list, fa_list_t list2) {}

/** Remove the given element from the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_remove(int begin, fa_list_t list) {}

/** Remove the given element from the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_dremove(int begin, fa_list_t list) {}

/** Remove the given range from the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_remove_range(int begin, int end, fa_list_t list) {}

/** Remove the given range from the given list.
    @par Performance
        O(n)
 */
fa_list_t fa_list_dremove_range(int begin, int end, fa_list_t list) {}


// --------------------------------------------------------------------------------
// Searching
// --------------------------------------------------------------------------------

/** Return whether the given list contains the given element.
    @par Performance
        O(n)
 */
bool fa_list_has(fa_ptr_t value, fa_list_t list) {}

/** Return the index of the first occurance given element in the
    list, or a negative value if no such element is found
    @par Performance
        O(n)
 */
int fa_list_index_of(fa_ptr_t value, fa_list_t list) {}

/** Return the first element satisfying the given predicate in the
    list, if found.
    @param list     List.
    @param value    Value to search for.
    @return         Index of the found value (optional).
    @par Performance
        O(log n)
 */
fa_ptr_t fa_list_find(fa_pred_t pred, fa_ptr_t data, fa_list_t list) {}

/** Return the index of the first element satisfying the given predicate in the
    list, or a negative value if no such element is found.
    @par Performance
        O(log n)
 */
int fa_list_find_index(fa_pred_t pred, fa_ptr_t data, fa_list_t list) {}



// --------------------------------------------------------------------------------
// Maps and folds
// --------------------------------------------------------------------------------

/** Return the result of applying the given function to all elements of the given list.

    @par Laws

        map(apply1, id, xs)                == xs
        map(apply1, f, map(apply1, g, xs)) == map(apply1, comp(f, g), xs)

    @par Performance
        O(n)
 */
list_t fa_list_map(fa_unary_t func, fa_ptr_t data, fa_list_t list) {}

/** Return the result of applying the given function to all elements of the given list.

    @par Laws

        map(apply1, id, xs)                == xs
        map(apply1, f, map(apply1, g, xs)) == map(apply1, comp(f, g), xs)

    @par Performance
        O(n)
 */
list_t fa_list_dmap(fa_unary_t func, fa_ptr_t data, fa_list_t list) {}

/** Return the given list with all elements not satisfying the given predicate removed.
    @par Performance
        O(n)
 */
list_t fa_list_filter(fa_pred_t pred, fa_ptr_t data, fa_list_t list) {}

/** Return the given list with all elements not satisfying the given predicate removed.
    @par Performance
        O(n)
 */
list_t fa_list_dfilter(fa_pred_t pred, fa_ptr_t data, fa_list_t list) {}

/** Fold over the given list from left to right.

    @par Performance
        O(n)
 */
fa_ptr_t fa_list_fold_left(fa_binary_t func,
                             fa_ptr_t    data,
                             fa_ptr_t    init,
                             fa_list_t   list) {}

/** Fold over the given list from left to right.

    @par Performance
        O(n)
 */
fa_ptr_t fa_list_dfold_left(fa_binary_t func,
                              fa_ptr_t    data,
                              fa_ptr_t    init,
                              fa_list_t   list) {}

/** Concatenate all elements of the given list.

    The given list must contain lists only.

    @par Performance
        O(n)
 */
list_t fa_list_join(fa_list_t lists) {}

/** Concatenate all elements of the given list.

    The given list must contain lists only.

    @par Performance
        O(n)
 */
list_t fa_list_djoin(fa_list_t lists) {}


/** Map over the given list and join the results.

    This function is useful to apply functions from singletons to lists.

    @par Laws

        joinMap(apply1, single, xs) == xs`

    @par Performance
        O(n)
 */
fa_list_t fa_list_join_map(fa_unary_t func,
                             fa_ptr_t data,
                             fa_list_t list) {}

/** Map over the given list and join the results.

    This function is useful to apply functions from singletons to lists.

    @par Laws

        joinMap(apply1, single, xs) == xs

    @par Performance
        O(n)
 */
fa_list_t fa_list_djoin_map(fa_unary_t func,
                              fa_ptr_t data,
                              fa_list_t list) {}


// --------------------------------------------------------------------------------

/** Create a list by repeating the given element.
 */
list_t fa_list_repeat(int times, fa_ptr_t value) {}

/** Create a list from the given range.
 */
list_t fa_list_enumerate(int begin, int end) {}


