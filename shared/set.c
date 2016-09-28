
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/set.h>
#include <fa/pair.h>
#include <fa/list.h>
#include <fa/string.h>
#include <fa/dynamic.h>
#include <fa/util.h>

/*
    ## Notes

    * Map is implemented in terms of Set

    * Set is implemented in terms of a (persistent) base type

        - Requires the operations defined below

        - We currently use ordinary lists

        - We really want fast size, insert, remove, indexOf
 */

#define base_t              fa_list_t
#define base_empty          fa_list_empty
#define base_copy           fa_list_copy
#define base_deep_copy      fa_list_deep_copy
#define base_destroy        fa_list_destroy
#define base_deep_destroy   fa_list_deep_destroy
#define base_insert         fa_list_insert
#define base_dinsert        fa_list_dinsert
#define base_remove         fa_list_remove
#define base_find           fa_list_find
#define base_index_of       fa_list_index_of
#define base_size           fa_list_length
#define base_to_list        fa_list_to_list

struct _fa_set_t {
    fa_impl_t       impl;       //  Interface dispatcher
    base_t          elems;
};


// --------------------------------------------------------------------------------

inline static fa_set_t new_set(base_t elems)
{
    fa_ptr_t set_impl(fa_id_t interface);

    fa_set_t set   = fa_new(set);
    set->impl   = &set_impl;
    set->elems  = elems;
    return set;
}

inline static void delete_set(fa_set_t set)
{
    fa_delete(set);
}

// --------------------------------------------------------------------------------

fa_set_t fa_set_empty()
{
    return new_set(base_empty());
}

fa_set_t fa_set_single(fa_ptr_t x)
{
    return fa_set_dadd(x, fa_set_empty());
}

fa_set_t fa_set_add(fa_ptr_t x, fa_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return new_set(base_insert((-i - 1), x, set->elems));
    } else {
        return fa_set_copy(set);
    }
}

fa_set_t fa_set_set(fa_ptr_t x, fa_set_t set)
{
    int i = base_index_of(x, set->elems);
    
    if (i < 0) {
        return new_set(base_insert((-i - 1), x, set->elems));
    } else {
        return new_set(base_insert(i, x, base_remove(i, set->elems)));
        // TODO should be base_dinsert?
    }
}

fa_set_t fa_set_remove(fa_ptr_t x, fa_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return fa_set_copy(set);
    } else {
        return new_set(base_remove(i, set->elems));
    }
}

fa_set_t fa_set_dadd(fa_ptr_t x, fa_set_t set)
{
    fa_set_t set2 = fa_set_add(x, set);
    fa_set_destroy(set);
    return set2;
}

fa_set_t fa_set_dset(fa_ptr_t x, fa_set_t set)
{
    fa_set_t set2 = fa_set_set(x, set);
    fa_set_destroy(set);
    return set2;
}

fa_set_t fa_set_dremove(fa_ptr_t x, fa_set_t set)
{
    fa_set_t set2 = fa_set_remove(x, set);
    fa_set_destroy(set);
    return set2;
}

fa_set_t fa_set_copy(fa_set_t set)
{
    return new_set(base_copy(set->elems));
}

fa_set_t fa_set_deep_copy(fa_set_t set)
{
    return new_set(base_deep_copy(set->elems));
}

void fa_set_destroy(fa_set_t set)
{
    base_destroy(set->elems);
    delete_set(set);
}

void fa_set_deep_destroy(fa_set_t set, fa_deep_destroy_pred_t pred)
{
    if (!(pred(set))) return;
    base_deep_destroy(set->elems, pred);
    delete_set(set);
}

// --------------------------------------------------------------------------------

bool fa_set_has(fa_ptr_t x, fa_set_t set)
{
    return base_index_of(x, set->elems) >= 0;
}

// bool eq(fa_ptr_t x, fa_ptr_t y) { return fa_equal(x, y); }
fa_ptr_t fa_set_get(fa_ptr_t x, fa_set_t set)
{
    return base_find(fa_equal, x, set->elems);
}

int fa_set_size(fa_set_t set)
{
    return base_size(set->elems);
}

bool fa_set_is_empty(fa_set_t set)
{
    return fa_set_size(set) == 0;
}

bool fa_set_is_single(fa_set_t set)
{
    return fa_set_size(set) == 1;
}

bool fa_set_is_subset_of(fa_set_t a, fa_set_t b)
{
    fa_for_each(x, base_to_list(a->elems)) {
        if (!fa_set_has(x, b)) {
            return false;
        }
    }
    return true;
}

bool fa_set_is_proper_subset_of(fa_set_t a, fa_set_t b)
{
    if (fa_set_size(a) >= fa_set_size(b)) {
        return false;
    }

    return fa_set_is_subset_of(a, b);
}

fa_set_t fa_set_sum(fa_set_t a, fa_set_t b)
{
    fa_set_t c = fa_set_copy(a);
    fa_for_each(x, base_to_list(b->elems)) {
        c = fa_set_dadd(x, c);
    }
    return c;
}

fa_set_t fa_set_intersection(fa_set_t a, fa_set_t b)
{
    fa_set_t c = fa_set_empty();
    fa_for_each(x, base_to_list(a->elems)) {
        if (fa_set_has(x, b)) {
            c = fa_set_dadd(x, c);
        }
    }
    return c;
}

fa_set_t fa_set_difference(fa_set_t a, fa_set_t b)
{
    fa_set_t c = fa_set_copy(a);
    fa_for_each(x, base_to_list(b->elems)) {
        c = fa_set_dremove(x, c);
    }
    return c;
}

fa_set_t fa_set_product(fa_set_t a, fa_set_t b)
{
    fa_set_t c = fa_set_empty();
    fa_for_each(x, base_to_list(a->elems)) {
        fa_for_each(y, base_to_list(b->elems)) {
            c = fa_set_dadd(fa_pair_create(x, y), c);
        }
    }
    return c;
}


// --------------------------------------------------------------------------------

fa_set_t fa_set(int count, ...)
{
    fa_set_t s = fa_set_empty();
    va_list args;

    va_start(args, count);

    for (int i = 0; i < count; ++i) {
        s = fa_set_dadd(va_arg(args, fa_ptr_t), s);
    }

    va_end(args);
    return s;
}

fa_list_t fa_set_to_list(fa_set_t set)
{
    return set->elems;
}

fa_set_t fa_set_from_list(fa_list_t elems)
{
    return new_set(elems);
}

// --------------------------------------------------------------------------------

bool set_equal(fa_ptr_t a, fa_ptr_t b)
{
    fa_set_t c = (fa_set_t) a;
    fa_set_t d = (fa_set_t) b;
    return fa_set_is_subset_of(c, d) && (fa_set_size(c) == fa_set_size(d));
}

bool set_less_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_set_t c = (fa_set_t) a;
    fa_set_t d = (fa_set_t) b;
    return fa_set_is_subset_of(c, d);
}

bool set_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_set_t c = (fa_set_t) a;
    fa_set_t d = (fa_set_t) b;
    return !fa_set_is_subset_of(c, d);
}

fa_string_t set_show(fa_ptr_t x)
{
    fa_set_t set = (fa_set_t) x;
    fa_string_t s  = fa_string("{");

    fa_for_each_last(value, base_to_list(set->elems), last) {
        s = fa_string_dappend(s, fa_string_show(value));

        if (!last) {
            s = fa_string_dappend(s, fa_string(","));
        }
    }
    s = fa_string_dappend(s, fa_string("}"));
    return s;
}

fa_ptr_t set_copy(fa_ptr_t a)
{
    return fa_set_copy(a);
}

fa_ptr_t set_deep_copy(fa_ptr_t a)
{
    return fa_set_copy(a);
}

void set_destroy(fa_ptr_t a)
{
    fa_set_destroy(a);
}

void set_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    fa_set_deep_destroy(a, p);
}

fa_dynamic_type_repr_t set_get_type(fa_ptr_t a)
{
    return set_type_repr;
}

fa_ptr_t set_impl(fa_id_t interface)
{
    static fa_equal_t set_equal_impl = { set_equal };
    static fa_string_show_t set_show_impl = { set_show };
    static fa_copy_t set_copy_impl = { set_copy, set_deep_copy };
    static fa_destroy_t set_destroy_impl = { set_destroy, set_deep_destroy };
    static fa_dynamic_t set_dynamic_impl = { set_get_type };

    switch (interface) {
    case fa_equal_i:
        return &set_equal_impl;

    case fa_string_show_i:
        return &set_show_impl;

    case fa_copy_i:
        return &set_copy_impl;

    case fa_destroy_i:
        return &set_destroy_impl;

    case fa_dynamic_i:
        return &set_dynamic_impl;

    default:
        return NULL;
    }
}

