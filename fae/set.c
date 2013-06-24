
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/set.h>
#include <fae/pair.h>
#include <fae/list.h>
#include <fae/string.h>
#include <fae/dynamic.h>
#include <fae/util.h>

/*  Notes:
        * Map is implemented in terms of Set
        * Set is implemented in terms of a (persistent) base type
            * Requires the operations defined below
            * We use lists until we have proper intmaps
            * We really want fast size, insert, remove, indexOf
 */

#define base_t              list_t
#define base_empty          fae_list_empty
#define base_copy           fae_list_copy
#define base_destroy        fae_list_destroy
#define base_insert         fae_list_insert
#define base_dinsert        fae_list_dinsert
#define base_remove         fae_list_remove
#define base_find           fae_list_find
#define base_index_of       fae_list_index_of
#define base_size           fae_list_length
#define base_to_list        fae_list_to_list

struct _fae_set_t {
    impl_t          impl;       //  Interface dispatcher
    base_t          elems;
};


// --------------------------------------------------------------------------------

inline static set_t new_set(base_t elems)
{
    fae_ptr_t set_impl(fae_id_t interface);

    set_t set   = fae_new(set);
    set->impl   = &set_impl;
    set->elems  = elems;
    return set;
}

inline static void delete_set(set_t set)
{
    fae_delete(set);
}

// --------------------------------------------------------------------------------

fae_set_t fae_set_empty()
{
    return new_set(base_empty());
}

fae_set_t fae_set_single(fae_ptr_t x)
{
    return fae_set_dadd(x, fae_set_empty());
}

fae_set_t fae_set_add(fae_ptr_t x, fae_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return new_set(base_insert((-i - 1), x, set->elems));
    } else {
        return fae_set_copy(set);
    }
}

fae_set_t fae_set_set(fae_ptr_t x, fae_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return new_set(base_insert((-i - 1), x, set->elems));
    } else {
        return new_set(base_insert(i, x, base_remove(i, set->elems)));
        // TODO should be base_dinsert?
    }
}

fae_set_t fae_set_remove(fae_ptr_t x, fae_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return fae_set_copy(set);
    } else {
        return new_set(base_remove(i, set->elems));
    }
}

fae_set_t fae_set_dadd(fae_ptr_t x, fae_set_t set)
{
    set_t set2 = fae_set_add(x, set);
    fae_set_destroy(set);
    return set2;
}

fae_set_t fae_set_dset(fae_ptr_t x, fae_set_t set)
{
    set_t set2 = fae_set_set(x, set);
    fae_set_destroy(set);
    return set2;
}

fae_set_t fae_set_dremove(fae_ptr_t x, fae_set_t set)
{
    set_t set2 = fae_set_remove(x, set);
    fae_set_destroy(set);
    return set2;
}

fae_set_t fae_set_copy(fae_set_t set)
{
    return new_set(base_copy(set->elems));
}

void fae_set_destroy(fae_set_t set)
{
    base_destroy(set->elems);
    delete_set(set);
}

// --------------------------------------------------------------------------------

bool fae_set_has(fae_ptr_t x, fae_set_t set)
{
    return base_index_of(x, set->elems) >= 0;
}

// bool eq(ptr_t x, ptr_t y) { return fae_equal(x, y); }
fae_ptr_t fae_set_get(fae_ptr_t x, fae_set_t set)
{
    return base_find(fae_equal, x, set->elems);
}

int fae_set_size(fae_set_t set)
{
    return base_size(set->elems);
}

bool fae_set_is_empty(fae_set_t set)
{
    return fae_set_size(set) == 0;
}

bool fae_set_is_single(fae_set_t set)
{
    return fae_set_size(set) == 1;
}

bool fae_set_is_subset_of(fae_set_t a, fae_set_t b)
{
    fae_for_each(x, base_to_list(a->elems)) {
        if (!fae_set_has(x, b)) {
            return false;
        }
    }
    return true;
}

bool fae_set_is_proper_subset_of(fae_set_t a, fae_set_t b)
{
    if (fae_set_size(a) >= fae_set_size(b)) {
        return false;
    }

    return fae_set_is_subset_of(a, b);
}

fae_set_t fae_set_sum(fae_set_t a, fae_set_t b)
{
    set_t c = fae_set_copy(a);
    fae_for_each(x, base_to_list(b->elems)) {
        c = fae_set_dadd(x, c);
    }
    return c;
}

fae_set_t fae_set_intersection(fae_set_t a, fae_set_t b)
{
    set_t c = fae_set_empty();
    fae_for_each(x, base_to_list(a->elems)) {
        if (fae_set_has(x, b)) {
            c = fae_set_dadd(x, c);
        }
    }
    return c;
}

fae_set_t fae_set_difference(fae_set_t a, fae_set_t b)
{
    set_t c = fae_set_copy(a);
    fae_for_each(x, base_to_list(b->elems)) {
        c = fae_set_dremove(x, c);
    }
    return c;
}

fae_set_t fae_set_product(fae_set_t a, fae_set_t b)
{
    set_t c = fae_set_empty();
    fae_for_each(x, base_to_list(a->elems)) {
        fae_for_each(y, base_to_list(b->elems)) {
            c = fae_set_dadd(pair(x, y), c);
        }
    }
    return c;
}

fae_set_t fae_set_power(fae_set_t set)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

/** Create a set from the given elements.
 */
set_t fae_set(int count, ...)
{
    set_t s = fae_set_empty();
    va_list args;

    va_start(args, count);

    for (int i = 0; i < count; ++i) {
        s = fae_set_dadd(va_arg(args, ptr_t), s);
    }

    va_end(args);
    return s;
}

list_t fae_set_to_list(set_t set)
{
    return set->elems;
}

// --------------------------------------------------------------------------------

bool set_equal(fae_ptr_t a, fae_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return fae_set_is_subset_of(c, d) && (fae_set_size(c) == fae_set_size(d));
}

bool set_less_than(fae_ptr_t a, fae_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return fae_set_is_subset_of(c, d);
}

bool set_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return !fae_set_is_subset_of(c, d);
}

fae_string_t set_show(fae_ptr_t x)
{
    set_t set = (set_t) x;
    string_t s  = string("{");

    fae_for_each_last(value, base_to_list(set->elems), last) {
        s = string_dappend(s, fae_string_show(value));

        if (!last) {
            s = string_dappend(s, string(","));
        }
    }
    s = string_dappend(s, string("}"));
    return s;
}

fae_ptr_t set_copy(fae_ptr_t a)
{
    return fae_set_copy(a);
}

void set_destroy(fae_ptr_t a)
{
    fae_set_destroy(a);
}

type_repr_t set_get_type(fae_ptr_t a)
{
    return set_type_repr;
}

fae_ptr_t set_impl(fae_id_t interface)
{
    static fae_equal_t set_equal_impl = { set_equal };
    static fae_string_show_t set_show_impl = { set_show };
    static fae_copy_t set_copy_impl = { set_copy };
    static fae_destroy_t set_destroy_impl = { set_destroy };
    static fae_dynamic_t set_dynamic_impl = { set_get_type };

    switch (interface) {
    case fae_equal_i:
        return &set_equal_impl;

    case fae_string_show_i:
        return &set_show_impl;

    case fae_copy_i:
        return &set_copy_impl;

    case fae_destroy_i:
        return &set_destroy_impl;

    case fae_dynamic_i:
        return &set_dynamic_impl;

    default:
        return NULL;
    }
}

