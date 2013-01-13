
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

/** Create an empty list.

    The returned list must be destroyed by passing to a destructive function.

    @return
        A new list.
    @note
        O(1)
 */
list_t doremir_list_empty() {}

/** Create a new list containing the given element.

    The returned list must be destroyed by passing to a destructive function.

    @param value
        Value to store.
    @return
        A new list.
    @note
        O(1)
 */
list_t doremir_list_single(doremir_ptr_t value) {}

/** Create a new list by inserting the given element at the beginning of the given list.

    The returned list must be destroyed by passing to a destructive function.

    @param head
        Value to form head of list.
    @param tail
        List to form tail of list.
    @return
        A new list.
    @note
        O(1)
 */
list_t doremir_list_cons(doremir_ptr_t head, doremir_list_t tail) {}

/** Create a new list by inserting the given element at the beginning of the given list.

    The returned list must be destroyed by passing to a destructive function.

    @param head
        Value to form head of list.
    @param tail
        List to form tail of list (destroyed).
    @return
        A new list.
    @note
        O(1)
 */
list_t doremir_list_dcons(doremir_ptr_t head, doremir_list_t tail) {}

/** Copy the given list.

    The returned list must be destroyed by passing to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_copy(doremir_list_t list) {}

/** Destroy the given list.

    @note
        O(n)
 */
void doremir_list_destroy(doremir_list_t list) {}

// --------------------------------------------------------------------------------

/** Return whether the given list is empty.
    @note
        O(1)
 */
bool doremir_list_is_empty(doremir_list_t list) {}

/** Return whether the given list has a single element.
    @note
        O(1)
 */
bool doremir_list_is_single(doremir_list_t list) {}

/** Return the lenght of the given list.
    @note
        O(n)
 */
int doremir_list_length(doremir_list_t list) {}


// --------------------------------------------------------------------------------
// Sequential access
// --------------------------------------------------------------------------------

/** Return the first element of the given list.
    @note
        O(1)
 */
doremir_ptr_t doremir_list_head(doremir_list_t list) {}

/** Return all elements but the first of the given list.
    @note
        O(1)
 */
doremir_list_t doremir_list_tail(doremir_list_t list) {}

/** Return all elements but the last of the given list.
    @note
        O(n)
 */
doremir_list_t doremir_list_init(doremir_list_t list) {}

/** Return all elements but the first of the given list.
    @note
        O(1)
 */
doremir_list_t doremir_list_dtail(doremir_list_t list) {}

/** Return all elements but the last of the given list.
    @note
        O(n)
 */
doremir_list_t doremir_list_dinit(doremir_list_t list) {}

/** Return the last element of the given list.
    @note
        O(n)
 */
doremir_ptr_t doremir_list_last(doremir_list_t list) {}


// --------------------------------------------------------------------------------
// Misc operations
// --------------------------------------------------------------------------------

/** Return the result of appending the given lists.
    @note
        O(n)
 */
list_t doremir_list_append(doremir_list_t list, doremir_list_t list2) {}

/** Return the result of appending the given lists.

    This function destroys both of the given lists.
    @note
        O(n)
 */
list_t doremir_list_dappend(doremir_list_t list, doremir_list_t list2) {}

/** Return the reverse of the given list.
    @note
        O(n)
 */
list_t doremir_list_reverse(doremir_list_t list) {}

/** Return the reverse of the given list.

    This function destroys the given list.
    @note
        O(n)
 */
list_t doremir_list_dreverse(doremir_list_t list) {}

/** Return the given list sorted.
    @note
        O(n log n)
 */
list_t doremir_list_sort(doremir_list_t list) {}

/** Return the given list sorted.

    This function destroys the given list.
    @note
        O(n log n)
 */
list_t doremir_list_dsort(doremir_list_t list) {}


// --------------------------------------------------------------------------------
// Random access
// --------------------------------------------------------------------------------

/** Return the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_take(int end, doremir_list_t list) {}

/** Return the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_dtake(int end, doremir_list_t list) {}

/** Return the all but the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_drop(int end, doremir_list_t list) {}

/** Return the all but the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_ddrop(int end, doremir_list_t list) {}

/** List index operator.
    @returns
        The nth element of the given list.
    @note
        O(n)
 */
doremir_ptr_t doremir_list_index(int end, doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_range(int begin, int end, doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_insert(int index, doremir_ptr_t value, doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_dinsert(int begin, doremir_ptr_t x, doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_insert_range(int begin, doremir_list_t list, doremir_list_t list2) {}
                                         
/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_dinsert_range(int begin, doremir_list_t list, doremir_list_t list2) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_remove(int begin, doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_dremove(int begin, doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_remove_range(int begin, int end, doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_dremove_range(int begin, int end, doremir_list_t list) {}


// --------------------------------------------------------------------------------
// Searching
// --------------------------------------------------------------------------------

/** Return whether the given list contains the given element.
    @note
        O(n)
 */
bool doremir_list_has(doremir_ptr_t value, doremir_list_t list) {}

/** Return the index of the first occurance given element in the
    list, or a negative value if no such element is found
    @note
        O(n)
 */
int doremir_list_index_of(doremir_ptr_t value, doremir_list_t list) {}

/** Return the first element satisfying the given predicate in the
    list, or null if no such element is found.
    @note
        O(log n)
 */
doremir_ptr_t doremir_list_find(doremir_pred_t pred, doremir_ptr_t data, doremir_list_t list) {}

/** Return the index of the first element satisfying the given predicate in the
    list, or a negative value if no such element is found.
    @note
        O(log n)
 */
int doremir_list_find_index(doremir_pred_t pred, doremir_ptr_t data, doremir_list_t list) {}



// --------------------------------------------------------------------------------
// Maps and folds
// --------------------------------------------------------------------------------

/** Return the result of applying the given function to all elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_map(doremir_unary_t func, doremir_ptr_t data, doremir_list_t list) {}

/** Return the result of applying the given function to all elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_dmap(doremir_unary_t func, doremir_ptr_t data, doremir_list_t list) {}

/** Return the given list with all elements not satisfying the given predicate removed.
    @note
        O(n)
 */
list_t doremir_list_filter(doremir_pred_t pred, doremir_ptr_t data, doremir_list_t list) {}

/** Return the given list with all elements not satisfying the given predicate removed.
    @note
        O(n)
 */
list_t doremir_list_dfilter(doremir_pred_t pred, doremir_ptr_t data, doremir_list_t list) {}

/** Return the result of applying the given function to all elements of the given list
    and the result of the previous such application, or the initial element for an empty
    list.

    @note
        O(n)
 */
doremir_ptr_t doremir_list_fold_left(doremir_binary_t func,
                                     doremir_ptr_t    data,
                                     doremir_ptr_t    init,
                                     doremir_list_t   list) {}

/** Fold over the given list.

    @note
        O(n)
 */
doremir_ptr_t doremir_list_dfold_left(doremir_binary_t func,
                                      doremir_ptr_t    data,
                                      doremir_ptr_t    init,
                                      doremir_list_t   list) {}

/** Return 
    @note
        O(n)
 */
list_t doremir_list_concat(doremir_list_t lists) {}

/** Return 
    @note
        O(n)
 */
list_t doremir_list_dconcat(doremir_list_t lists) {}


/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_concat_map(doremir_unary_t func, 
                                       doremir_ptr_t data,
                                       doremir_list_t list) {}

/** Return 
    @note
        O(n)
 */
doremir_list_t doremir_list_dconcat_map(doremir_unary_t func,
                                        doremir_ptr_t data,
                                        doremir_list_t list) {}


// --------------------------------------------------------------------------------

/** Create a list by repeating the given element.
    @see
        list in \ref doremir/util.h
 */
list_t doremir_list_repeat(int times, doremir_ptr_t value) {}

/** Create a list from the given range.
    @see
        list in \ref doremir/util.h
 */
list_t doremir_list_enum_from(int begin, int end) {}


