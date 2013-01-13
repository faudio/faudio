
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

/** Create a new empty list.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_empty() {}

/** Create a new list containing the given element.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_single(doremir_ptr_t x) {}

/** Create a new list by inserting the given element at the beginning of the given list.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_cons(doremir_ptr_t x, doremir_list_t xs) {}

/** Create a new list by inserting the given element at the end of the given list.
    This function destroys the given list.

    @note
        O(1)
 */
list_t doremir_list_dcons(doremir_ptr_t x, doremir_list_t xs) {}

/** Copy the given list.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_copy(doremir_list_t xs) {}

/** Destroy the given list.

    @note
        O(n)
 */
void doremir_list_destroy(doremir_list_t xs) {}

// --------------------------------------------------------------------------------

/** Returns whether the given list is empty.
    @note
        O(1)
 */
bool doremir_list_is_empty(doremir_list_t xs) {}

/** Returns whether the given list has a single element.
    @note
        O(1)
 */
bool doremir_list_is_single(doremir_list_t xs) {}

/** Returns the lenght of the given list.
    @note
        O(n)
 */
int doremir_list_length(doremir_list_t xs) {}


// --------------------------------------------------------------------------------
// Sequential access
// --------------------------------------------------------------------------------

/** Returns the first element of the given list.
    @note
        O(1)
 */
doremir_ptr_t doremir_list_head(doremir_list_t xs) {}

/** Returns all elements but the first of the given list.
    @note
        O(1)
 */
doremir_list_t doremir_list_tail(doremir_list_t xs) {}

/** Returns all elements but the last of the given list.
    @note
        O(n)
 */
doremir_list_t doremir_list_init(doremir_list_t xs) {}

/** Returns all elements but the first of the given list, which is destroyed.
    @note
        O(1)
 */
doremir_list_t doremir_list_dtail(doremir_list_t xs) {}

/** Returns all elements but the last of the given list, which is destroyed.
    @note
        O(n)
 */
doremir_list_t doremir_list_dinit(doremir_list_t xs) {}

/** Returns the last element of the given list.
    @note
        O(n)
 */
doremir_ptr_t doremir_list_last(doremir_list_t xs) {}


// --------------------------------------------------------------------------------
// Misc operations
// --------------------------------------------------------------------------------

/** Returns the result of appending the given lists.
    @note
        O(n)
 */
list_t doremir_list_append(doremir_list_t xs, doremir_list_t ys) {}

/** Returns the result of appending the given lists.

    This function destroys both of the given lists.
    @note
        O(n)
 */
list_t doremir_list_dappend(doremir_list_t xs, doremir_list_t ys) {}

/** Returns the reverse of the given list.
    @note
        O(n)
 */
list_t doremir_list_reverse(doremir_list_t xs) {}

/** Returns the reverse of the given list.

    This function destroys the given list.
    @note
        O(n)
 */
list_t doremir_list_dreverse(doremir_list_t xs) {}

/** Returns the given list sorted.
    @note
        O(n log n)
 */
list_t doremir_list_sort(doremir_list_t xs) {}

/** Returns the given list sorted.

    This function destroys the given list.
    @note
        O(n log n)
 */
list_t doremir_list_dsort(doremir_list_t xs) {}


// --------------------------------------------------------------------------------
// Random access
// --------------------------------------------------------------------------------

/** Returns the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_take(int n, doremir_list_t xs) {}

/** Returns the *n* leading elements of the given list, which is destroyed.
    @note
        O(n)
 */
list_t doremir_list_dtake(int n, doremir_list_t xs) {}

/** Returns the all but the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_drop(int n, doremir_list_t xs) {}

/** Returns the all but the *n* leading elements of the given list, which is destroyed.
    @note
        O(n)
 */
list_t doremir_list_ddrop(int n, doremir_list_t xs) {}

/** Returns the *n*-th elements of the given list, or NULL.
    @note
        O(n)
 */
doremir_ptr_t doremir_list_index(int n, doremir_list_t xs) {}

doremir_list_t doremir_list_range(int m, int n, doremir_list_t xs) {}

doremir_list_t doremir_list_insert(int index, doremir_ptr_t value, doremir_list_t list) {}

doremir_list_t doremir_list_dinsert(int m, doremir_ptr_t x, doremir_list_t xs) {}

doremir_list_t doremir_list_insert_range(int m, doremir_list_t xs, doremir_list_t ys) {}
                                         
doremir_list_t doremir_list_dinsert_range(int m,
                                          doremir_list_t xs,
                                          doremir_list_t ys) {}

doremir_list_t doremir_list_remove(int m, doremir_list_t xs) {}

doremir_list_t doremir_list_dremove(int m, doremir_list_t xs) {}

doremir_list_t doremir_list_remove_range(int m, int n, doremir_list_t xsx) {}

doremir_list_t doremir_list_dremove_range(int m,
                                          int n,
                                          doremir_list_t xs) {}


// --------------------------------------------------------------------------------
// Searching
// --------------------------------------------------------------------------------

/** Returns whether the given list contains the given element.
    @note
        O(n)
 */
bool doremir_list_has(doremir_ptr_t value, doremir_list_t list) {}

/** Returns the first element satisfying the given predicate in the
    given list, or a negative value if no such element is found.
    @note
        O(log n)
 */
doremir_ptr_t doremir_list_find(doremir_pred_t pred, doremir_list_t list) {}

/** Returns the index of the first element satisfying the given predicate in the
    given list, or a negative value if no such element is found.
    @note
        O(log n)
 */
int doremir_list_find_index(doremir_pred_t pred, doremir_list_t list) {}


// --------------------------------------------------------------------------------
// Maps and folds
// --------------------------------------------------------------------------------

/** Returns the result of applying the given function to all elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_map(doremir_unary_t f, doremir_list_t xs) {}

list_t doremir_list_dmap(doremir_unary_t f, doremir_list_t xs) {}

doremir_list_t doremir_list_concat_map(doremir_unary_t f,
                                       doremir_list_t xs) {}

doremir_list_t doremir_list_dconcat_map(doremir_unary_t f,
                                        doremir_list_t xs) {}

/** Returns the given list with all elements not satisfying the given predicate removed.
    @note
        O(n)
 */
list_t doremir_list_filter(doremir_pred_t p, doremir_list_t xs) {}

list_t doremir_list_dfilter(doremir_pred_t p, doremir_list_t xs) {}

/** Returns the result of applying the given function to all elements of the given list
    and the result of the previous such application, or the initial element for an empty
    list.

    @note
        O(n)
 */
doremir_ptr_t doremir_list_fold_left(doremir_binary_t f,
                                     doremir_ptr_t    z,
                                     doremir_list_t   xs) {}

doremir_ptr_t doremir_list_dfold_left(doremir_binary_t f,
                                      doremir_ptr_t    z,
                                      doremir_list_t   xs) {}

list_t doremir_list_concat(doremir_list_t xss) {}


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
list_t doremir_list_enum_from(int m, int n) {}


