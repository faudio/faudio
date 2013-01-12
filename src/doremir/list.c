
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/list.h>
#include <doremir/util.h>
#include <doremir/string.h>

/*
    TODO
        insert
        sort
        find
        map
        concat

    Notes:
        * The list type is a wrapper structure containing the dispatcher.
        * Below that is straightforward immutable, singly-linked node sequence
        * For memory management we use structural sharing with one reference count per node.
            * Pros: Copying a list is O(1)
            * Cons: No destructive optimizations, all destrutive methods are wrappers

    Possibilities:
        * Add a "transient list" type with no sharing.
          This gives us "as good as mutable" destrucive operations but slow copy.
        * For faster random access (important for use as backend to set map etc), add a hash
          trie implementation.
 */

struct node {
        size_t          count;      //  Number of references
        struct node     * next;     //  Next node or null
        ptr_t           value;      //  The value
    };

typedef struct node *node_t;

struct _doremir_list_t {
        impl_t          impl;       //  Interface dispatcher
        node_t          node;       //  Top-level node
    };

inline static
node_t new_node(doremir_ptr_t value, node_t next)
{
    node_t node = doremir_new_struct(node);
    node->count = 1;
    node->value = value;
    node->next  = next;
    return node;
}

inline static
node_t take_node(node_t node)
{
    if (node)
        node->count++;      /* TODO make atomic? */
    return node;
}

inline static
void release_node(node_t node)
{
    if (!node) return;

    node->count--;
    if (node->count == 0)
    {
        release_node(node->next);
        doremir_delete(node);
    }
}

doremir_ptr_t list_impl(doremir_id_t interface);

inline static
list_t new_list(node_t node)
{
    list_t list = doremir_new(list);
    list->impl = &list_impl;
    list->node = node;
    return list;
}

inline static
bool has_node(list_t list)
{
    return list->node;
}

inline static
bool has_head(list_t list)
{
    return list->node && list->node->value;
}

inline static
bool has_tail(list_t list)
{
    return list->node && list->node->next;
}

inline static
void delete_list(list_t list)
{
    doremir_delete(list);
}



// --------------------------------------------------------------------------------
// Constructors
// --------------------------------------------------------------------------------

/** Create a new empty list.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_empty()
{
    list_t xs = new_list(NULL);
    return xs;
}

/** Create a new list containing the given element.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_single(doremir_ptr_t x)
{
    list_t xs = new_list(new_node(x, NULL));
    return xs;
}

/** Create a new list by inserting the given element at the beginning of the given list.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(1)
 */
list_t doremir_list_cons(doremir_ptr_t x, doremir_list_t xs)
{
    list_t ys = new_list(new_node(x, take_node(xs->node)));
    return ys;
}

/** Create a new list by inserting the given element at the end of the given list.
    This function destroys the given list.

    @note
        O(1)
 */
list_t doremir_list_dcons(doremir_ptr_t x, doremir_list_t xs)
{
    list_t ys = new_list(new_node(x, xs->node));
    delete_list(xs);
    return ys;
}

/** Copy the given list.

    Lists have single-ownership semantics and must be finalized by passing it
    to a destructive function.

    @note
        O(n)
 */
list_t doremir_list_copy(doremir_list_t xs)
{
    list_t ys = new_list(take_node(xs->node));
    return ys;
}

/** Destroy the given list.

    @note
        O(n)
 */
void doremir_list_destroy(doremir_list_t xs)
{
    release_node(xs->node);
    delete_list(xs);
}

/** Create a list from the given elements.
    @see
        list in \ref doremir/util.h
 */
list_t doremir_list(int count, ...)
{
    node_t node = NULL, *next = &node;
    {
        va_list args;
        va_start(args, count);

        for (int i = 0; i < count; ++i)
        {
             // Append a node
            *next = new_node(va_arg(args, ptr_t), NULL);

             // Update next to point to the created node
            next = &((*next)->next);
        }
        va_end(args);
    }
    return new_list(node);
}

// --------------------------------------------------------------------------------

/** Returns whether the given list is empty.
    @note
        O(1)
 */
bool doremir_list_is_empty(doremir_list_t xs)
{
    return !has_node(xs);
}

/** Returns whether the given list has a single element.
    @note
        O(1)
 */
bool doremir_list_is_single(doremir_list_t xs)
{
    return has_node(xs) && !has_tail(xs);
}

/** Returns the lenght of the given list.
    @note
        O(n)
 */
int doremir_list_length(doremir_list_t xs)
{
    int    count = 0;
    node_t node  = xs->node;
    while (node)
    {
        count++;
        node = node->next;
    }
    return count;
}

/** Returns the first element of the given list.
    @note
        O(1)
 */
doremir_ptr_t doremir_list_head(doremir_list_t xs)
{
    if (!has_node(xs))
        assert(false && "No head");
    return xs->node->value;
}

/** Returns all elements but the first of the given list.
    @note
        O(1)
 */
doremir_list_t doremir_list_tail(doremir_list_t xs)
{
    if (!has_node(xs))
        assert(false && "No tail");
    if(!xs->node->next)
        return empty();
    else
        return new_list(take_node(xs->node->next));
}

/** Returns all elements but the first of the given list, which is destroyed.
    @note
        O(1)
 */
doremir_list_t doremir_list_dtail(doremir_list_t xs)
{
    doremir_list_t ys = doremir_list_tail(xs);
    doremir_list_destroy(xs);
    return ys;
}

// --------------------------------------------------------------------------------

/** Returns all elements but the last of the given list.
    @note
        O(n)
 */
doremir_list_t doremir_list_init(doremir_list_t xs)
{
    if (is_empty(xs))
        assert(false && "No init");
    if (is_single(xs))
        return empty();
    return cons(head(xs), init(xs));
}

/** Returns the last element of the given list.
    @note
        O(1)
 */
doremir_ptr_t doremir_list_last(doremir_list_t xs)
{
    if (is_empty(xs))
        assert(false && "No last");
    if (is_single(xs))
        return head(xs);
    return last(xs);
}

/** Returns the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_take(int n, doremir_list_t xs)
{
    if (n <= 0 || is_empty(xs))
        return empty();
    return cons(head(xs), take(n - 1, xs));
}

/** Returns the all but the *n* leading elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_drop(int n, doremir_list_t xs)
{
    if (n <= 0 || is_empty(xs))
        return empty();
    return drop(n - 1, xs);
}

doremir_list_t doremir_list_range(int m, int n, doremir_list_t xs)
{
    return doremir_list_dtake(n, doremir_list_drop(m, xs));
}

doremir_list_t doremir_list_remove_range(int m, int n, doremir_list_t xsx)
{
    list_t xs = doremir_list_take(m,     xs);
    list_t ys = doremir_list_drop(m + n, xs);
    return doremir_list_dappend(xs, ys);
}


/** Returns whether the given list contains the given element.
    @note
        O(n)
 */
bool doremir_list_has_elem(doremir_ptr_t x, doremir_list_t xs)
{
    return eq(x, head(xs)) || has_elem(x, xs);
}





/** Returns all elements but the last of the given list, which is destroyed.
    @note
        O(n)
 */
doremir_list_t doremir_list_dinit(doremir_list_t xs)
{
    doremir_list_t ys = doremir_list_init(xs);
    doremir_list_destroy(xs);
    return ys;
}

/** Returns the *n* leading elements of the given list, which is destroyed.
    @note
        O(n)
 */
list_t doremir_list_dtake(int n, doremir_list_t xs)
{
    doremir_list_t ys = doremir_list_take(n, xs);
    doremir_list_destroy(xs);
    return ys;
}

/** Returns the all but the *n* leading elements of the given list, which is destroyed.
    @note
        O(n)
 */
list_t doremir_list_ddrop(int n, doremir_list_t xs)
{
    doremir_list_t ys = doremir_list_drop(n, xs);
    doremir_list_destroy(xs);
    return ys;
}

doremir_list_t doremir_list_insert(int index, doremir_ptr_t value, doremir_list_t list)
{
    assert(false && "Not implemented");
}



// --------------------------------------------------------------------------------

static inline list_t _append(list_t xs, list_t ys)
{
    if (is_empty(xs))
        return ys;
    else
        return cons(head(xs), _append(tail(xs), ys));
}
static inline list_t _revappend(list_t xs, list_t ys)
{
    if (is_empty(xs))
        return ys;
    else
        return _revappend(tail(xs), cons(head(xs), ys));
}



/** Returns the reverse of the given list.
    @note
        O(n)
 */
list_t doremir_list_reverse(doremir_list_t xs)
{
    return _revappend(xs, empty());
}

/** Returns the result of appending the given lists.
    @note
        O(n)
 */
list_t doremir_list_append(doremir_list_t xs, doremir_list_t ys)
{
    return _append(xs, ys);
}

/** Returns the result of appending the given lists, which are both destroyed.
    @note
        O(n)
 */
list_t doremir_list_dappend(doremir_list_t xs, doremir_list_t ys)
{
    list_t zs = _append(xs, ys);
    doremir_list_destroy(xs);
    doremir_list_destroy(ys);
    return zs;
}


static inline list_t _sort(list_t xs)
{
    // list_t small = doremir_list_filter();// TODO
    // list_t large = doremir_list_filter();
    // return append(_sort(small), cons(head(xs), _sort(large)));
    assert(false && "Not implemented");
}

/** Returns the given list sorted.
    @note
        O(n log n)
 */
list_t doremir_list_sort(doremir_list_t xs)
{
    return _sort(xs);
}

/** Returns the first element satisfying the given predicate in the
    given list, or a negative value if no such element is found.
    @note
        O(log n)
 */
doremir_ptr_t doremir_list_find(doremir_pred_t p, doremir_list_t xs)
{
    assert(false && "Not implemented");
}

/** Returns the index of the first element satisfying the given predicate in the
    given list, or a negative value if no such element is found.
    @note
        O(log n)
 */
int doremir_list_find_index(doremir_pred_t p, doremir_list_t xs)
{
    assert(false && "Not implemented");
}

/** Returns the given list with all elements not satisfying the given predicate removed.
    @note
        O(n)
 */
list_t doremir_list_filter(doremir_pred_t p, doremir_list_t xs)
{
    node_t n, *np;
    node_t xn = xs->node;
    n  = NULL;
    np = &n;
    while(xn)
    {
        if (p(xn->value))
        {
            *np = new_node(xn->value,NULL);
            np = &(*np)->next;
        }
        xn = xn->next;
    }
    return new_list(n);
}


// --------------------------------------------------------------------------------

/** Returns the result of applying the given function to all elements of the given list.
    @note
        O(n)
 */
list_t doremir_list_map(doremir_unary_t f, doremir_list_t xs)
{
    assert(false && "Not implemented");
}

/** Returns the result of applying the given function to all elements of the given list
    and the result of the previous such application, or the initial element for an empty
    list.

    @note
        O(n)
 */
doremir_ptr_t doremir_list_fold_left(doremir_binary_t f,
                                     doremir_ptr_t    z,
                                     doremir_list_t   xs)
{
    doremir_ptr_t v = z;
    node_t xn = xs->node;
    while(xn)
    {
        v  = f(v, xn->value);
        xn = xn->next;
    }
    return v;
}

list_t doremir_list_concat(doremir_list_t xss)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

bool list_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    node_t an = ((list_t) a)->node;
    node_t bn = ((list_t) b)->node;
    while (an && bn)
    {
        if (!eq(an->value, bn->value))
            return false;
        an = an->next;
        bn = bn->next;
    }
    return !(an || bn);
}

bool list_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    node_t an = ((list_t) a)->node;
    node_t bn = ((list_t) b)->node;
    while (an && bn)
    {
        if (lt(an->value, bn->value))
            return true;
        if (gt(an->value, bn->value))
            return false;
        an = an->next;
        bn = bn->next;
    }
    return bn && !an;
}

bool list_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    node_t an = ((list_t) a)->node;
    node_t bn = ((list_t) b)->node;
    while (an && bn)
    {
        if (gt(an->value, bn->value))
            return true;
        if (lt(an->value, bn->value))
            return false;
        an = an->next;
        bn = bn->next;
    }
    return an && !bn;
}

doremir_string_t list_show(doremir_ptr_t xs)
{
    string_t s  = string("[");
    node_t   xn = ((list_t) xs)->node;
    while(xn)
    {
        s = string_dappend(s, sshow(xn->value));
        xn = xn->next;
        if (xn)
            s = string_dappend(s, string(","));
    };
    s = string_dappend(s, string("]"));
    return s;
}

doremir_ptr_t list_copy(doremir_ptr_t a)
{
    return doremir_list_copy(a);
}

void list_destroy(doremir_ptr_t a)
{
    doremir_list_destroy(a);
}

doremir_ptr_t list_impl(doremir_id_t interface)
{
    static doremir_equal_t list_equal_impl = { list_equal };
    static doremir_order_t list_order_impl = { list_less_than, list_greater_than };
    static doremir_string_show_t list_show_impl = { list_show };
    static doremir_copy_t list_copy_impl = { list_copy };
    static doremir_destroy_t list_destroy_impl = { list_destroy };

    switch (interface)
    {
    case doremir_equal_i:
        return &list_equal_impl;

    case doremir_order_i:
        return &list_order_impl;

    case doremir_string_show_i:
        return &list_show_impl;

    case doremir_copy_i:
        return &list_copy_impl;

    case doremir_destroy_i:
        return &list_destroy_impl;

    default:
        return NULL;
    }
}

