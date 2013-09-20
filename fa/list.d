
/*
    FAE
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
list_t fae_list_empty() {}

/** Create a new list containing the given element.

    The returned list must be destroyed by passing to a destructive function.

    @param value
        Value to store.
    @return
        A new list.
    @par Performance
        O(1)
 */
list_t fae_list_single(fae_ptr_t value) {}

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
list_t fae_list_cons(fae_ptr_t head, fae_list_t tail) {}

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
list_t fae_list_dcons(fae_ptr_t head, fae_list_t tail) {}

/** Copy the given list.

    The returned list must be destroyed by passing to a destructive function.

    @par Performance
        O(1)
 */
list_t fae_list_copy(fae_list_t list) {}

/** Destroy the given list.

    @par Performance
        O(n)
 */
void fae_list_destroy(fae_list_t list) {}

// --------------------------------------------------------------------------------

/** Return whether the given list is empty.
    @par Performance
        O(1)
 */
bool fae_list_is_empty(fae_list_t list) {}

/** Return whether the given list has a single element.
    @par Performance
        O(1)
 */
bool fae_list_is_single(fae_list_t list) {}

/** Return the lenght of the given list.
    @par Performance
        O(n)
 */
int fae_list_length(fae_list_t list) {}


// --------------------------------------------------------------------------------
// Sequential access
// --------------------------------------------------------------------------------

/** Return the first element of the given list.
    @par Performance
        O(1)
 */
fae_ptr_t fae_list_head(fae_list_t list) {}

/** Return all elements but the first of the given list.
    @par Performance
        O(1)
 */
fae_list_t fae_list_tail(fae_list_t list) {}

/** Return all elements but the last of the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_init(fae_list_t list) {}

/** Return all elements but the first of the given list.
    @par Performance
        O(1)
 */
fae_list_t fae_list_dtail(fae_list_t list) {}

/** Return all elements but the last of the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_dinit(fae_list_t list) {}

/** Return the last element of the given list.
    @par Performance
        O(n)
 */
fae_ptr_t fae_list_last(fae_list_t list) {}


// --------------------------------------------------------------------------------
// Misc operations
// --------------------------------------------------------------------------------

/** Return the result of appending the given lists.
    @par Performance
        O(n)
 */
list_t fae_list_append(fae_list_t list, fae_list_t list2) {}

/** Return the result of appending the given lists.

    This function destroys both of the given lists.
    @par Performance
        O(n)
 */
list_t fae_list_dappend(fae_list_t list, fae_list_t list2) {}

/** Return the reverse of the given list.
    @par Performance
        O(n)
 */
list_t fae_list_reverse(fae_list_t list) {}

/** Return the reverse of the given list.

    This function destroys the given list.
    @par Performance
        O(n)
 */
list_t fae_list_dreverse(fae_list_t list) {}

/** Return the given list sorted.
    @par Performance
        O(n log n)
 */
list_t fae_list_sort(fae_list_t list) {}

/** Return the given list sorted.

    This function destroys the given list.
    @par Performance
        O(n log n)
 */
list_t fae_list_dsort(fae_list_t list) {}


// --------------------------------------------------------------------------------
// Random access
// --------------------------------------------------------------------------------

/** Return the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fae_list_take(int end, fae_list_t list) {}

/** Return the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fae_list_dtake(int end, fae_list_t list) {}

/** Return the all but the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fae_list_drop(int end, fae_list_t list) {}

/** Return the all but the *n* leading elements of the given list.
    @par Performance
        O(n)
 */
list_t fae_list_ddrop(int end, fae_list_t list) {}

/** List index operator.
    @returns
        The nth element of the given list.
    @par Performance
        O(n)
 */
fae_ptr_t fae_list_index(int end, fae_list_t list) {}

/** Return the given range of the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_range(int begin, int end, fae_list_t list) {}

/** Insert the given element into the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_insert(int index, fae_ptr_t value, fae_list_t list) {}

/** Insert the given element into the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_dinsert(int begin, fae_ptr_t x, fae_list_t list) {}

/** Insert the given range into the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_insert_range(int begin, fae_list_t list, fae_list_t list2) {}

/** Insert the given range into the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_dinsert_range(int begin, fae_list_t list, fae_list_t list2) {}

/** Remove the given element from the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_remove(int begin, fae_list_t list) {}

/** Remove the given element from the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_dremove(int begin, fae_list_t list) {}

/** Remove the given range from the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_remove_range(int begin, int end, fae_list_t list) {}

/** Remove the given range from the given list.
    @par Performance
        O(n)
 */
fae_list_t fae_list_dremove_range(int begin, int end, fae_list_t list) {}


// --------------------------------------------------------------------------------
// Searching
// --------------------------------------------------------------------------------

/** Return whether the given list contains the given element.
    @par Performance
        O(n)
 */
bool fae_list_has(fae_ptr_t value, fae_list_t list) {}

/** Return the index of the first occurance given element in the
    list, or a negative value if no such element is found
    @par Performance
        O(n)
 */
int fae_list_index_of(fae_ptr_t value, fae_list_t list) {}

/** Return the first element satisfying the given predicate in the
    list, if found.
    @param list     List.
    @param value    Value to search for.
    @return         Index of the found value (optional).
    @par Performance
        O(log n)
 */
fae_ptr_t fae_list_find(fae_pred_t pred, fae_ptr_t data, fae_list_t list) {}

/** Return the index of the first element satisfying the given predicate in the
    list, or a negative value if no such element is found.
    @par Performance
        O(log n)
 */
int fae_list_find_index(fae_pred_t pred, fae_ptr_t data, fae_list_t list) {}



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
list_t fae_list_map(fae_unary_t func, fae_ptr_t data, fae_list_t list) {}

/** Return the result of applying the given function to all elements of the given list.

    @par Laws

        map(apply1, id, xs)                == xs
        map(apply1, f, map(apply1, g, xs)) == map(apply1, comp(f, g), xs)

    @par Performance
        O(n)
 */
list_t fae_list_dmap(fae_unary_t func, fae_ptr_t data, fae_list_t list) {}

/** Return the given list with all elements not satisfying the given predicate removed.
    @par Performance
        O(n)
 */
list_t fae_list_filter(fae_pred_t pred, fae_ptr_t data, fae_list_t list) {}

/** Return the given list with all elements not satisfying the given predicate removed.
    @par Performance
        O(n)
 */
list_t fae_list_dfilter(fae_pred_t pred, fae_ptr_t data, fae_list_t list) {}

/** Fold over the given list from left to right.

    @par Performance
        O(n)
 */
fae_ptr_t fae_list_fold_left(fae_binary_t func,
                             fae_ptr_t    data,
                             fae_ptr_t    init,
                             fae_list_t   list) {}

/** Fold over the given list from left to right.

    @par Performance
        O(n)
 */
fae_ptr_t fae_list_dfold_left(fae_binary_t func,
                              fae_ptr_t    data,
                              fae_ptr_t    init,
                              fae_list_t   list) {}

/** Concatenate all elements of the given list.

    The given list must contain lists only.

    @par Performance
        O(n)
 */
list_t fae_list_join(fae_list_t lists) {}

/** Concatenate all elements of the given list.

    The given list must contain lists only.

    @par Performance
        O(n)
 */
list_t fae_list_djoin(fae_list_t lists) {}


/** Map over the given list and join the results.

    This function is useful to apply functions from singletons to lists.

    @par Laws

        joinMap(apply1, single, xs) == xs`

    @par Performance
        O(n)
 */
fae_list_t fae_list_join_map(fae_unary_t func,
                             fae_ptr_t data,
                             fae_list_t list) {}

/** Map over the given list and join the results.

    This function is useful to apply functions from singletons to lists.

    @par Laws

        joinMap(apply1, single, xs) == xs

    @par Performance
        O(n)
 */
fae_list_t fae_list_djoin_map(fae_unary_t func,
                              fae_ptr_t data,
                              fae_list_t list) {}


// --------------------------------------------------------------------------------

/** Create a list by repeating the given element.
 */
list_t fae_list_repeat(int times, fae_ptr_t value) {}

/** Create a list from the given range.
 */
list_t fae_list_enumerate(int begin, int end) {}


