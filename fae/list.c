
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/list.h>
#include <fae/string.h>
#include <fae/dynamic.h>
#include <fae/util.h>

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
    struct node    *next;       //  Next node or null
    ptr_t           value;      //  The value
};

typedef struct node *node_t;

struct _fae_list_t {
    impl_t          impl;       //  Interface dispatcher
    node_t          node;       //  Top-level node
};


// --------------------------------------------------------------------------------

/** Create a new node with a single reference.
 */
inline static node_t new_node(ptr_t value, node_t next)
{
    node_t node = fae_new_struct(node);
    node->count = 1;
    node->value = value;
    node->next  = next;
    return node;
}

/** Add a reference to the given node.
 */
inline static node_t take_node(node_t node)
{
    if (node) {
        node->count++;  // TODO make atomic
    }

    return node;
}

/** Remove a reference from the given node, deleting
    if there are no more references.
 */
inline static void release_node(node_t node)
{
    if (!node) {
        return;
    }

    node->count--;

    if (node->count == 0) {
        release_node(node->next);
        fae_delete(node);
    }
}

ptr_t list_impl(fae_id_t interface);

inline static list_t new_list(node_t node)
{
    list_t list = fae_new(list);
    list->impl = &list_impl;
    list->node = node;
    return list;
}

inline static void delete_list(list_t list)
{
    fae_delete(list);
}

/** Iterate over the nodes of a list. The variable var will be a node_t referencing
    the node in the following block.

    impl_for_each_node(my_list, node)
        fae_print("%s\n", node->value);

 */
#define impl_for_each_node(list, var) \
    for(node_t _n = list->node; _n; _n = _n->next) \
        fae_let(var, _n)

/** Iterate over the elements of a list. The variable var will be a ptr_t
    referencing the value in the following block.

    This macro is independent from the foreach in <fae/utils.h>, which should
    not be used in this file.

    impl_for_each_node(my_list, value)
        fae_print("%s\n", value);

 */
#define impl_for_each(list, var) \
    for(node_t _n = list->node; _n; _n = _n->next) \
        fae_let(var, _n->value)

/** The begin_node, append_node and prepend_node macros can be used to construct
    a list in place.

    For example:

        begin_node(node, next);
        while (...)
          if (...)
            append_node(next, value);
          else
            prepend_node(next, value);
        return new_list(node);

    @param place
        A node_t pointer.
    @param value
        Value to add.

 */

#define begin_node(var, next) \
    node_t var = NULL, *next = &var

#define append_node(place, value) \
    do {                                        \
        *place = new_node(value, NULL);         \
        place = &(*place)->next;                \
    } while (0)

#define prepend_node(place, value) \
    do {                                        \
        *place = new_node(value, *place);       \
    } while (0)


// --------------------------------------------------------------------------------

list_t fae_list_empty()
{
    return new_list(NULL);
}

list_t fae_list_single(ptr_t x)
{
    return new_list(new_node(x, NULL));
}

list_t fae_list_copy(list_t xs)
{
    return new_list(take_node(xs->node));
}

list_t fae_list_cons(ptr_t x, list_t xs)
{
    return new_list(new_node(x, take_node(xs->node)));
}

list_t fae_list_dcons(ptr_t x, list_t xs)
{
    list_t ys = new_list(new_node(x, xs->node));
    delete_list(xs);
    return ys;
}

void fae_list_destroy(list_t xs)
{
    release_node(xs->node);
    delete_list(xs);
}

bool fae_list_is_empty(list_t xs)
{
    return !xs->node;
}

bool fae_list_is_single(list_t xs)
{
    return xs->node && !xs->node->next;
}

int fae_list_length(list_t xs)
{
    int count = 0;
    impl_for_each(xs, value) {
        value = 0; // kill warning
        count++;
    }
    return count;
}


// --------------------------------------------------------------------------------

ptr_t fae_list_head(list_t xs)
{
    if (!xs->node) {
        assert(false && "No head");
    }

    return xs->node->value;
}

list_t fae_list_tail(list_t xs)
{
    if (!xs->node) {
        assert(false && "No tail");
    }

    return new_list(take_node(xs->node->next));
}

list_t fae_list_init(list_t xs)
{
    if (!xs->node) {
        assert(false && "No init");
    }

    begin_node(node, next);
    impl_for_each_node(xs, node) {
        if (node->next) {
            append_node(next, node->value);
        }
    }
    return new_list(node);
}

ptr_t fae_list_last(list_t xs)
{
    impl_for_each_node(xs, node) {
        if (!node->next) {
            return node->value;
        }
    }
    assert(false && "No last");
}

list_t fae_list_dtail(list_t xs)
{
    list_t ys = fae_list_tail(xs);
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_dinit(list_t xs)
{
    list_t ys = fae_list_init(xs);
    fae_list_destroy(xs);
    return ys;
}


// --------------------------------------------------------------------------------

static inline list_t append(list_t xs, list_t ys)
{
    if (is_empty(xs)) {
        return fae_list_copy(ys);
    } else {
        list_t xst = fae_list_tail(xs);
        list_t r = fae_list_dcons(fae_list_head(xs), append(xst, ys));
        fae_list_destroy(xst);
        return r;
    }
}

static inline list_t revappend(list_t xs, list_t ys)
{
    if (is_empty(xs)) {
        return fae_list_copy(ys);
    } else {
        list_t xst = fae_list_tail(xs);
        list_t con = fae_list_cons(fae_list_head(xs), ys);
        list_t r = revappend(xst, con);
        fae_list_destroy(xst);
        fae_list_destroy(con);
        return r;
    }
}

list_t fae_list_append(list_t xs, list_t ys)
{
    return append(xs, ys);
}

list_t fae_list_reverse(list_t xs)
{
    return revappend(xs, fae_list_empty());
}

static inline list_t merge(list_t xs, list_t ys)
{
    begin_node(node, next);

    while (!fae_list_is_empty(xs) && !fae_list_is_empty(ys)) {
        ptr_t x, y;

        x = fae_list_head(xs);
        y = fae_list_head(ys);

        if (fae_less_than(x, y)) {
            append_node(next, x);
            xs = fae_list_tail(xs);
        } else {
            append_node(next, y);
            ys = fae_list_tail(ys);
        }
    }

    if (!fae_list_is_empty(xs)) {
        return fae_list_append(new_list(node), xs);
    }

    if (!fae_list_is_empty(ys)) {
        return fae_list_append(new_list(node), ys);
    }

    return new_list(node);
}

static inline list_t dmerge(list_t xs, list_t ys)
{
    list_t res = merge(xs, ys);
    fae_list_destroy(xs);
    fae_list_destroy(ys);
    return res;
}


static inline list_t dmerge_sort(list_t xs)
{
    int len, mid;
    list_t left, right;

    len = fae_list_length(xs);
    mid = len / 2;

    if (len <= 1) {
        return xs;
    }

    left  = fae_list_take(mid, xs);
    right = fae_list_ddrop(mid, xs); // xs destroyed here

    left  = dmerge_sort(left);
    right = dmerge_sort(right);

    if (fae_less_than(fae_list_last(left),
                      fae_list_head(right))) {
        return fae_list_dappend(left, right);
    } else {
        return dmerge(left, right);
    }
}

list_t fae_list_sort(list_t xs)
{
    return dmerge_sort(fae_list_copy(xs));
}

list_t fae_list_dappend(list_t xs, list_t ys)
{
    list_t zs = append(xs, ys);
    fae_list_destroy(xs);
    fae_list_destroy(ys);
    return zs;
}

list_t fae_list_dreverse(list_t xs)
{
    list_t ys = revappend(xs, fae_list_empty());
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_dsort(list_t xs)
{
    list_t ys = dmerge_sort(xs);
    return ys;
}


// --------------------------------------------------------------------------------

list_t fae_list_take(int n, list_t xs)
{
    if (n <= 0 || fae_list_is_empty(xs)) {
        return fae_list_empty();
    }

    return fae_list_dcons(fae_list_head(xs), fae_list_dtake(n - 1, fae_list_tail(xs)));
}

list_t fae_list_drop(int n, list_t xs)
{
    if (n < 0 || fae_list_is_empty(xs)) {
        return fae_list_empty();
    }

    if (n == 0) {
        return fae_list_copy(xs);
    }

    return fae_list_ddrop(n - 1, fae_list_tail(xs));
}

ptr_t fae_list_index(int n, list_t xs)
{
    int i = 0;
    impl_for_each(xs, x) {
        if (i++ == n) {
            return x;
        }
    }
    return NULL;
}

list_t fae_list_range(int m, int n, list_t xs)
{
    return fae_list_dtake(n, fae_list_drop(m, xs));
}

list_t fae_list_remove_range(int m, int n, list_t xs)
{
    list_t as = fae_list_take(m,     xs);
    list_t bs = fae_list_drop(m + n, xs);
    return fae_list_dappend(as, bs);
}

list_t fae_list_insert_range(int m, list_t xs, list_t ys)
{
    list_t as = fae_list_take(m, ys);
    list_t bs = fae_list_copy(xs);
    list_t cs = fae_list_drop(m, ys);
    return fae_list_dappend(as, fae_list_dappend(bs, cs));
}

list_t fae_list_insert(int index, ptr_t value, list_t list)
{
    list_t elem = fae_list_single(value);
    list_t res  = fae_list_insert_range(index, elem, list);
    fae_list_destroy(elem);
    return res;
}

list_t fae_list_remove(int index, list_t list)
{
    return fae_list_remove_range(index, 1, list);
}

list_t fae_list_dtake(int n, list_t xs)
{
    list_t ys = fae_list_take(n, xs);
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_ddrop(int n, list_t xs)
{
    list_t ys = fae_list_drop(n, xs);
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_dinsert(int m, ptr_t x, list_t xs)
{
    list_t ys = fae_list_insert(m, x, xs);
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_dinsert_range(int m, list_t xs, list_t ys)
{
    list_t zs = fae_list_insert_range(m, xs, ys);
    fae_list_destroy(xs);
    fae_list_destroy(ys);
    return zs;
}

list_t fae_list_dremove(int m, list_t xs)
{
    list_t ys = fae_list_remove(m, xs);
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_dremove_range(int m, int n, list_t xs)
{
    list_t ys = fae_list_remove_range(m, n, xs);
    fae_list_destroy(xs);
    return ys;
}


// --------------------------------------------------------------------------------

bool fae_list_has(ptr_t value, list_t list)
{
    impl_for_each(list, elem) {
        if (fae_equal(value, elem)) {
            return true;
        }
    }
    return false;
}

int fae_list_index_of(ptr_t value, list_t list)
{
    int index = 0;
    impl_for_each(list, elem) {
        if (fae_equal(elem, value)) {
            return index;
        }

        if (fae_greater_than(elem, value)) {
            break;
        }

        index++;
    }
    return -(index + 1);
}

ptr_t fae_list_find(pred_t pred, ptr_t data, list_t list)
{
    impl_for_each(list, elem) {
        if (pred(data, elem)) {
            return elem;
        }
    }
    return NULL;
}

int fae_list_find_index(pred_t pred, ptr_t data, list_t list)
{
    int index = 0;
    impl_for_each(list, elem) {
        if (pred(data, elem)) {
            return index;
        }

        index++;
    }
    return -(index + 1);
}


// --------------------------------------------------------------------------------

list_t fae_list_map(unary_t func, ptr_t data, list_t list)
{
    begin_node(node, next);
    impl_for_each(list, elem) {
        append_node(next, func(data, elem));
    }
    return new_list(node);
}

list_t fae_list_filter(pred_t pred, ptr_t data, list_t list)
{
    begin_node(node, next);
    impl_for_each(list, elem) {
        if (pred(data, elem)) {
            append_node(next, elem);
        }
    }
    return new_list(node);
}

ptr_t fae_list_fold_left(binary_t func, ptr_t data, ptr_t init, list_t list)
{
    ptr_t value = init;
    impl_for_each(list, elem) {
        value = func(data, value, elem);
    }
    return value;
}

list_t fae_list_join(list_t list)
{
    list_t result = empty();
    impl_for_each(list, elem) {
        result = fae_list_dappend(result, fae_list_copy(elem));
    }
    return result;
}

list_t fae_list_join_map(unary_t func, ptr_t data, list_t list)
{
    list_t ys = fae_list_map(func, data, list);
    return fae_list_djoin(ys);
}

list_t fae_list_dmap(unary_t f, ptr_t d, list_t xs)
{
    list_t ys = fae_list_map(f, d, xs);
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_dfilter(pred_t p, ptr_t d, list_t xs)
{
    list_t ys = fae_list_filter(p, d, xs);
    fae_list_destroy(xs);
    return ys;
}

ptr_t fae_list_dfold_left(binary_t f, ptr_t d, ptr_t  z, list_t   xs)
{
    list_t ys = fae_list_fold_left(f, d, z, xs);
    fae_list_destroy(xs);
    return ys;
}

list_t fae_list_djoin(list_t list)
{
    list_t ys = fae_list_join(list);
    fae_list_destroy(list);
    return ys;
}

list_t fae_list_djoin_map(unary_t f, ptr_t d, list_t xs)
{
    list_t ys = fae_list_join_map(f, d, xs);
    fae_list_destroy(xs);
    return ys;
}


// --------------------------------------------------------------------------------

list_t fae_list(int count, ...)
{
    va_list args;
    va_start(args, count);
    begin_node(node, next);

    for (int i = 0; i < count; ++i) {
        append_node(next, va_arg(args, ptr_t));
    }

    va_end(args);
    return new_list(node);
}

list_t fae_list_repeat(int times, ptr_t value)
{
    begin_node(node, next);

    for (int i = 0; i < times; ++i) {
        append_node(next, value);
    }

    return new_list(node);
}

list_t fae_list_enumerate(int m, int n)
{
    begin_node(node, next);

    for (int i = 0; i < n; ++i) {
        append_node(next, m + i32(i));
    }

    return new_list(node);
}

list_t fae_list_to_list(list_t list)
{
    return fae_list_copy(list);
}


// --------------------------------------------------------------------------------

bool list_equal(ptr_t a, ptr_t b)
{
    node_t an = ((list_t) a)->node;
    node_t bn = ((list_t) b)->node;

    while (an && bn) {
        if (!fae_equal(an->value, bn->value)) {
            return false;
        }

        an = an->next;
        bn = bn->next;
    }

    return !(an || bn);
}

bool list_less_than(ptr_t a, ptr_t b)
{
    node_t an = ((list_t) a)->node;
    node_t bn = ((list_t) b)->node;

    while (an && bn) {
        if (fae_less_than(an->value, bn->value)) {
            return true;
        }

        if (fae_greater_than(an->value, bn->value)) {
            return false;
        }

        an = an->next;
        bn = bn->next;
    }

    return bn && !an;
}

bool list_greater_than(ptr_t a, ptr_t b)
{
    node_t an = ((list_t) a)->node;
    node_t bn = ((list_t) b)->node;

    while (an && bn) {
        if (fae_greater_than(an->value, bn->value)) {
            return true;
        }

        if (fae_less_than(an->value, bn->value)) {
            return false;
        }

        an = an->next;
        bn = bn->next;
    }

    return an && !bn;
}

fae_string_t list_show(ptr_t xs)
{
    string_t s  = string("");
    node_t   xn = ((list_t) xs)->node;

    s = string_dappend(s, string("["));

    while (xn) {
        s = string_dappend(s, fae_string_show(xn->value));
        xn = xn->next;

        if (xn) {
            s = string_dappend(s, string(","));
        }
    };

    s = string_dappend(s, string("]"));

    return s;
}

ptr_t list_copy(ptr_t a)
{
    return fae_list_copy(a);
}

void list_destroy(ptr_t a)
{
    fae_list_destroy(a);
}

type_repr_t list_get_type(fae_ptr_t a)
{
    return list_type_repr;
}

ptr_t list_impl(fae_id_t interface)
{
    static fae_equal_t list_equal_impl = { list_equal };
    static fae_order_t list_order_impl = { list_less_than, list_greater_than };
    static fae_string_show_t list_show_impl = { list_show };
    static fae_copy_t list_copy_impl = { list_copy };
    static fae_destroy_t list_destroy_impl = { list_destroy };
    static fae_dynamic_t list_dynamic_impl = { list_get_type };

    switch (interface) {
    case fae_equal_i:
        return &list_equal_impl;

    case fae_order_i:
        return &list_order_impl;

    case fae_string_show_i:
        return &list_show_impl;

    case fae_copy_i:
        return &list_copy_impl;

    case fae_destroy_i:
        return &list_destroy_impl;

    case fae_dynamic_i:
        return &list_dynamic_impl;

    default:
        return NULL;
    }
}

