
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/set.h>
#include <doremir/pair.h>
#include <doremir/list.h>
#include <doremir/string.h>
#include <doremir/dynamic.h>
#include <doremir/util.h>

/*  Notes:
        * Map is implemented in terms of Set
        * Set is implemented in terms of a (persistent) base type
            * Requires the operations defined below
            * We use lists until we have proper intmaps
            * We really want fast size, insert, remove, indexOf
 */

#define base_t              list_t
#define base_empty          doremir_list_empty
#define base_copy           doremir_list_copy
#define base_destroy        doremir_list_destroy
#define base_insert         doremir_list_insert
#define base_dinsert        doremir_list_dinsert
#define base_remove         doremir_list_remove
#define base_find           doremir_list_find
#define base_index_of       doremir_list_index_of
#define base_size           doremir_list_length
#define base_to_list        doremir_list_to_list

struct _doremir_set_t {
    impl_t          impl;       //  Interface dispatcher
    base_t          elems;
};

// --------------------------------------------------------------------------------

inline static set_t new_set(base_t elems)
{
    doremir_ptr_t set_impl(doremir_id_t interface);

    set_t set   = doremir_new(set);
    set->impl   = &set_impl;
    set->elems  = elems;
    return set;
}

inline static void delete_set(set_t set)
{
    doremir_delete(set);
}

// --------------------------------------------------------------------------------

doremir_set_t doremir_set_empty()
{
    return new_set(base_empty());
}

doremir_set_t doremir_set_single(doremir_ptr_t x)
{
    return doremir_set_dadd(x, doremir_set_empty());
}

doremir_set_t doremir_set_add(doremir_ptr_t x, doremir_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return new_set(base_insert((-i - 1), x, set->elems));
    } else {
        return doremir_set_copy(set);
    }
}

doremir_set_t doremir_set_set(doremir_ptr_t x, doremir_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return new_set(base_insert((-i - 1), x, set->elems));
    } else {
        return new_set(base_insert(i, x, base_remove(i, set->elems)));
        // TODO should be base_dinsert?
    }
}

doremir_set_t doremir_set_remove(doremir_ptr_t x, doremir_set_t set)
{
    int i = base_index_of(x, set->elems);

    if (i < 0) {
        return doremir_set_copy(set);
    } else {
        return new_set(base_remove(i, set->elems));
    }
}

doremir_set_t doremir_set_dadd(doremir_ptr_t x, doremir_set_t set)
{
    set_t set2 = doremir_set_add(x, set);
    doremir_set_destroy(set);
    return set2;
}

doremir_set_t doremir_set_dset(doremir_ptr_t x, doremir_set_t set)
{
    set_t set2 = doremir_set_set(x, set);
    doremir_set_destroy(set);
    return set2;
}

doremir_set_t doremir_set_dremove(doremir_ptr_t x, doremir_set_t set)
{
    set_t set2 = doremir_set_remove(x, set);
    doremir_set_destroy(set);
    return set2;
}

doremir_set_t doremir_set_copy(doremir_set_t set)
{
    return new_set(base_copy(set->elems));
}

void doremir_set_destroy(doremir_set_t set)
{
    base_destroy(set->elems);
    delete_set(set);
}

// --------------------------------------------------------------------------------

bool doremir_set_has(doremir_ptr_t x, doremir_set_t set)
{
    return base_index_of(x, set->elems) >= 0;
}

// bool eq(ptr_t x, ptr_t y) { return doremir_equal(x, y); }
doremir_ptr_t doremir_set_get(doremir_ptr_t x, doremir_set_t set)
{
    return base_find(doremir_equal, x, set->elems);
}

int doremir_set_size(doremir_set_t set)
{
    return base_size(set->elems);
}

bool doremir_set_is_empty(doremir_set_t set)
{
    return doremir_set_size(set) == 0;
}

bool doremir_set_is_single(doremir_set_t set)
{
    return doremir_set_size(set) == 1;
}

bool doremir_set_is_subset_of(doremir_set_t a, doremir_set_t b)
{
    doremir_for_each(x, base_to_list(a->elems)) {
        if (!doremir_set_has(x, b)) {
            return false;
        }
    }
    return true;
}

bool doremir_set_is_proper_subset_of(doremir_set_t a, doremir_set_t b)
{
    if (doremir_set_size(a) >= doremir_set_size(b)) {
        return false;
    }

    return doremir_set_is_subset_of(a, b);
}

doremir_set_t doremir_set_sum(doremir_set_t a, doremir_set_t b)
{
    set_t c = doremir_set_copy(a);
    doremir_for_each(x, base_to_list(b->elems)) {
        c = doremir_set_dadd(x, c);
    }
    return c;
}

doremir_set_t doremir_set_intersection(doremir_set_t a, doremir_set_t b)
{
    set_t c = doremir_set_empty();
    doremir_for_each(x, base_to_list(a->elems)) {
        if (doremir_set_has(x, b));

        c = doremir_set_dadd(x, c);
    }
    return c;
}

doremir_set_t doremir_set_difference(doremir_set_t a, doremir_set_t b)
{
    set_t c = doremir_set_copy(a);
    doremir_for_each(x, base_to_list(b->elems)) {
        c = doremir_set_dremove(x, c);
    }
    return c;
}

doremir_set_t doremir_set_product(doremir_set_t a, doremir_set_t b)
{
    set_t c = doremir_set_empty();
    doremir_for_each(x, base_to_list(a->elems)) {
        doremir_for_each(y, base_to_list(b->elems)) {
            c = doremir_set_dadd(pair(x, y), c);
        }
    }
    return c;
}

doremir_set_t doremir_set_power(doremir_set_t set)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

/** Create a set from the given elements.
 */
set_t doremir_set(int count, ...)
{
    set_t s = doremir_set_empty();
    va_list args;

    va_start(args, count);

    for (int i = 0; i < count; ++i) {
        s = doremir_set_dadd(va_arg(args, ptr_t), s);
    }

    va_end(args);
    return s;
}

list_t doremir_set_to_list(set_t set)
{
    return set->elems;
}

// --------------------------------------------------------------------------------

bool set_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return doremir_set_is_subset_of(c, d) && (doremir_set_size(c) == doremir_set_size(d));
}

bool set_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return doremir_set_is_subset_of(c, d);
}

bool set_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return !doremir_set_is_subset_of(c, d);
}

doremir_string_t set_show(doremir_ptr_t x)
{
    set_t set = (set_t) x;
    string_t s  = string("{");

    doremir_for_each_last(value, base_to_list(set->elems), last) {
        s = string_dappend(s, doremir_string_show(value));

        if (!last) {
            s = string_dappend(s, string(","));
        }
    }
    s = string_dappend(s, string("}"));
    return s;
}

doremir_ptr_t set_copy(doremir_ptr_t a)
{
    return doremir_set_copy(a);
}

void set_destroy(doremir_ptr_t a)
{
    doremir_set_destroy(a);
}

type_repr_t set_get_type(doremir_ptr_t a)
{
    return set_type_repr;
}

doremir_ptr_t set_impl(doremir_id_t interface)
{
    static doremir_equal_t set_equal_impl = { set_equal };
    static doremir_string_show_t set_show_impl = { set_show };
    static doremir_copy_t set_copy_impl = { set_copy };
    static doremir_destroy_t set_destroy_impl = { set_destroy };
    static doremir_dynamic_t set_dynamic_impl = { set_get_type };

    switch (interface) {
    case doremir_equal_i:
        return &set_equal_impl;

    case doremir_string_show_i:
        return &set_show_impl;

    case doremir_copy_i:
        return &set_copy_impl;

    case doremir_destroy_i:
        return &set_destroy_impl;

    case doremir_dynamic_i:
        return &set_dynamic_impl;

    default:
        return NULL;
    }
}

