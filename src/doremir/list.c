
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/list.h>
#include <doremir/string.h>
#include <doremir/util.h>

/*
    Notes:
        * The list type is a wrapper structure containing the dispatcher.
        * Below that is straightforward immutable, singly-linked node sequence
        * For memory management we use structural sharing with one reference count per node.
            * Pros: Copying a list is O(1)
            * Cons: No destructive optimizations, all destrutive methods are wrappers

    Possibilities:
        * Add a "transient list" type with no sharing.
          This gives us "as good as mutable" destrucive operations but slow copy.
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

/** Create a new node with a single reference.
 */
inline static
node_t new_node(ptr_t value, node_t next)
{
    node_t node = doremir_new_struct(node);
    node->count = 1;
    node->value = value;
    node->next  = next;
    return node;
}

/** Add a reference to the given node.
 */
inline static
node_t take_node(node_t node)
{
    if (node)
        node->count++;      /* TODO make atomic? */
    return node;
}

/** Remove a reference from the given node, deleting
    if there are no more references.
 */
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

ptr_t list_impl(doremir_id_t interface);

inline static
list_t new_list(node_t node)
{
    list_t list = doremir_new(list);
    list->impl = &list_impl;
    list->node = node;
    return list;
}

inline static
void delete_list(list_t list)
{
    doremir_delete(list);
}

#define for_each_node(list, var) \
    for(node_t _n = list->node; _n; _n = _n->next) \
        doremir_let(node_t, var = _n)

#define for_each(list, var) \
    for(node_t _n = list->node; _n; _n = _n->next) \
        doremir_let(ptr_t, var = _n->value)

/** Allocate a new node containing the given value, store it
    in the given place, then update the place to reference
    the next field of the created node.

    This can be used to construct a list in place, like

        node_t node = NULL, *next = &node;
        while (...)
            append_node(next, value);

    @param place
        A node_t pointer.
    @param value
        Value to add.

 */
#define append_node(place,value) \
    do {                                \
        *place = new_node(value, NULL); \
        place = &((*place)->next);      \
    } while (0)


// --------------------------------------------------------------------------------
// Constructors
// --------------------------------------------------------------------------------

list_t doremir_list_empty()
{
    return new_list(NULL);
}

list_t doremir_list_single(ptr_t x)
{
    return new_list(new_node(x, NULL));
}

list_t doremir_list_cons(ptr_t x, list_t xs)
{
    return new_list(new_node(x, take_node(xs->node)));
}

list_t doremir_list_copy(list_t xs)
{
    return new_list(take_node(xs->node));
}

void doremir_list_destroy(list_t xs)
{
    release_node(xs->node);
    delete_list(xs);
}

list_t doremir_list_dcons(ptr_t x, list_t xs)
{
    list_t ys = new_list(new_node(x, xs->node));
    delete_list(xs);
    return ys;
}


// --------------------------------------------------------------------------------
// Predicates
// --------------------------------------------------------------------------------

bool doremir_list_is_empty(list_t xs)
{
    return !xs->node;
}

bool doremir_list_is_single(list_t xs)
{
    return xs->node && !xs->node->next;
}

int doremir_list_length(list_t xs)
{
    int count = 0;
    for_each (xs, value)
    {
        value = NULL;   // kill warning
        count++;
    }
    return count;
}


// --------------------------------------------------------------------------------
// Sequential access
// --------------------------------------------------------------------------------

ptr_t doremir_list_head(list_t xs)
{
    if (!xs->node)
        assert(false && "No head");
    
    return xs->node->value;
}

list_t doremir_list_tail(list_t xs)
{
    if (!xs->node)
        assert(false && "No tail");
    
    return new_list(take_node(xs->node->next));
}

list_t doremir_list_init(list_t xs)
{
    if (!xs->node)
        assert(false && "No init");

    node_t node = NULL, *next = &node;

    for_each_node (xs, node)
    {
        if (node->next)
            append_node(next, node->value);
    }
    return new_list(node);
}

ptr_t doremir_list_last(list_t xs)
{
    for_each_node (xs, node)
    {
        if (!node->next)
            return node->value;
    }
    assert(false && "No tail");
}

list_t doremir_list_dtail(list_t xs)
{
    list_t ys = doremir_list_tail(xs);
    doremir_list_destroy(xs);
    return ys;
}

list_t doremir_list_dinit(list_t xs)
{
    list_t ys = doremir_list_init(xs);
    doremir_list_destroy(xs);
    return ys;
}




// --------------------------------------------------------------------------------
// Misc operations
// --------------------------------------------------------------------------------

static inline
list_t base_append(list_t xs, list_t ys)
{
    if (is_empty(xs))
        return ys;
    else
        return cons(head(xs), base_append(tail(xs), ys));
}

static inline
list_t base_revappend(list_t xs, list_t ys)
{
    if (is_empty(xs))
        return ys;
    else
        return base_revappend(tail(xs), cons(head(xs), ys));
}

list_t doremir_list_append(list_t xs, list_t ys)
{
    return base_append(xs, ys);
}

list_t doremir_list_reverse(list_t xs)
{
    return base_revappend(xs, empty());
}

static inline
list_t base_sort(list_t xs)
{
    // list_t small = doremir_list_filter();// TODO
    // list_t large = doremir_list_filter();
    // return append(base_sort(small), cons(head(xs), base_sort(large)));
    assert(false && "Not implemented");
}

list_t doremir_list_sort(list_t xs)
{
    return base_sort(xs);
}



list_t doremir_list_dappend(list_t xs, list_t ys)
{
    list_t zs = base_append(xs, ys);
    doremir_list_destroy(xs);
    doremir_list_destroy(ys);
    return zs;
}

list_t doremir_list_dreverse(list_t xs)
{
    list_t ys = base_revappend(xs, empty());
    doremir_list_destroy(xs);
    return ys;
}

list_t doremir_list_dsort(list_t xs)
{
    list_t ys = base_sort(xs);
    doremir_list_destroy(xs);
    return ys;
}


// --------------------------------------------------------------------------------
// Random access
// --------------------------------------------------------------------------------

list_t doremir_list_take(int n, list_t xs)
{
    if (n <= 0 || is_empty(xs))
        return empty();
    return cons(head(xs), take(n - 1, xs));
}

list_t doremir_list_dtake(int n, list_t xs)
{
    list_t ys = doremir_list_take(n, xs);
    doremir_list_destroy(xs);
    return ys;
}

list_t doremir_list_drop(int n, list_t xs)
{
    if (n <= 0 || is_empty(xs))
        return empty();
    return drop(n - 1, xs);
}

list_t doremir_list_ddrop(int n, list_t xs)
{
    list_t ys = doremir_list_drop(n, xs);
    doremir_list_destroy(xs);
    return ys;
}

ptr_t doremir_list_index(int n, list_t xs)
{
    int i = 0;
    for_each (xs, x)
    {
        if (i++ == n)
            return x;
    }
    return NULL;
}

list_t doremir_list_range(int m, int n, list_t xs)
{
    return doremir_list_dtake(n, doremir_list_drop(m, xs));
}

list_t doremir_list_insert(int index, ptr_t value, list_t list)
{
    assert(false && "Not implemented");
}

list_t doremir_list_dinsert(int m, ptr_t x, list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_insert_range(int m, list_t xs, list_t ys)
{
    assert(false && "Not implemented");
}

list_t doremir_list_dinsert_range(int m, list_t xs, list_t ys)
{
    assert(false && "Not implemented");
}

list_t doremir_list_remove(int m, list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_dremove(int m, list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_remove_range(int m, int n, list_t xsx)
{
    list_t xs = doremir_list_take(m,     xs);
    list_t ys = doremir_list_drop(m + n, xs);
    return doremir_list_dappend(xs, ys);
}

list_t doremir_list_dremove_range(int m, int n, list_t xs)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------
// Searching
// --------------------------------------------------------------------------------

bool doremir_list_has(ptr_t value, list_t list)
{
    for_each (list, elem)
    {
        if (doremir_equal(value, elem))
            return true;
    }
    return false;
}

int doremir_list_index_of(ptr_t value, list_t list)
{
    int index = 0;
    for_each (list, elem)
    {
        if (doremir_equal(value, elem))
            return index;
        index++;
    }
    return -(index + 1);
}

ptr_t doremir_list_find(pred_t pred, list_t list)
{
    for_each (list, elem)
    {
        if (pred(elem))
            return elem;
    }
    return NULL;
}

int doremir_list_find_index(pred_t pred, list_t list)
{
    int index = 0;
    for_each (list, elem)
    {
        if (pred(elem))
            return index;
        index++;
    }
    return -(index + 1);
}


// --------------------------------------------------------------------------------
// Maps and folds
// --------------------------------------------------------------------------------

list_t doremir_list_map(unary_t func, list_t list)
{
    node_t node = NULL, *next = &node;

    for_each (list, x)
    {
        append_node(next, func(x));
    }
    return new_list(node);
}

list_t doremir_list_concat_map(unary_t func, list_t list)
{
    list_t ys = doremir_list_map(func, list);
    return doremir_list_dconcat(ys);
}

list_t doremir_list_filter(pred_t pred, list_t list)
{
    node_t node = NULL, *next = &node;

    for_each (list, elem)
    {
        if (pred(elem))
            append_node(next, elem);
    }
    return new_list(node);
}

ptr_t doremir_list_fold_left(binary_t func, ptr_t init, list_t list)
{
    ptr_t value = init;

    for_each (list, elem)
    {
        value = func(value, elem);
    }
    return value;
}

list_t doremir_list_concat(list_t list)
{
    assert(false && "Not implemented");
}

list_t doremir_list_dmap(unary_t f, list_t xs)
{
    list_t ys = doremir_list_map(f, xs);
    doremir_list_destroy(xs);
    return ys;
}

list_t doremir_list_dconcat_map(unary_t f, list_t xs)
{
    list_t ys = doremir_list_concat_map(f, xs);
    doremir_list_destroy(xs);
    return ys;
}

list_t doremir_list_dfilter(pred_t p, list_t xs)
{
    list_t ys = doremir_list_filter(p, xs);
    doremir_list_destroy(xs);
    return ys;
}

ptr_t doremir_list_dfold_left(binary_t f, ptr_t  z, list_t   xs)
{
    list_t ys = doremir_list_fold_left(f, z, xs);
    doremir_list_destroy(xs);
    return ys;
}

list_t doremir_list_dconcat(list_t list)
{
    list_t ys = doremir_list_concat(list);
    doremir_list_destroy(list);
    return ys;
}


// --------------------------------------------------------------------------------

/** Create a list from the given elements.
    @see
        list in \ref doremir/util.h
 */
list_t doremir_list(int count, ...)
{
    list_t l = doremir_list_empty();
    va_list args;

    va_start(args, count);

    for (int i = 0; i < count; ++i)
        l = doremir_list_dcons(va_arg(args, ptr_t), l);

    va_end(args);
    return l;
}

list_t doremir_list_repeat(int times, ptr_t value)
{
    node_t node = NULL, *next = &node;

    for (int i = 0; i < times; ++i)
        append_node(next, value);

    return new_list(node);
}

list_t doremir_list_enum_from(int m, int n)
{
    node_t node = NULL, *next = &node;

    for (int i = 0; i < n; ++i)
        append_node(next, (ptr_t) m + i);

    return new_list(node);
}

// --------------------------------------------------------------------------------

bool list_equal(ptr_t a, ptr_t b)
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

bool list_less_than(ptr_t a, ptr_t b)
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

bool list_greater_than(ptr_t a, ptr_t b)
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

doremir_string_t list_show(ptr_t xs)
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

ptr_t list_copy(ptr_t a)
{
    return doremir_list_copy(a);
}

void list_destroy(ptr_t a)
{
    doremir_list_destroy(a);
}

ptr_t list_impl(doremir_id_t interface)
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

