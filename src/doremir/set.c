
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/set.h>
#include <doremir/pair.h>
#include <doremir/list.h>
#include <doremir/string.h>
#include <doremir/util.h>

/*  Notes:
        * Map is implemented in terms of Set
        * Set is implemented in terms of a base type
            * Requires the operations defined below
            * We use lists until we have persistent vectors
 */

#define base_t              list_t
#define base_empty          doremir_list_empty
#define base_copy           doremir_list_copy
#define base_destroy        doremir_list_destroy
#define base_insert         doremir_list_insert
#define base_remove         doremir_list_remove
#define base_index_of       doremir_list_index_of
#define base_length         doremir_list_length
#define base_equal          doremir_equal
#define base_less_than      doremir_less_than
#define base_greater_than   doremir_greater_than
#define base_for_each       doremir_list_for_each

struct _doremir_set_t {
        impl_t          impl;       //  Interface dispatcher
        base_t          elems;
    };

doremir_ptr_t set_impl(doremir_id_t interface);

inline static set_t
new_set(base_t elems)
{
    set_t set   = doremir_new(set);
    set->impl   = &set_impl;
    set->elems  = elems;
    return set;
}

inline static void
delete_set(set_t set)
{
    doremir_delete(set);
}

// --------------------------------------------------------------------------------

doremir_set_t doremir_set_empty()
{
    return new_set(base_empty());
}

doremir_set_t doremir_set_add(doremir_ptr_t x, doremir_set_t set)
{                             
    int i = base_index_of(x, set->elems);
    if (i >= 0)
        return doremir_set_copy(set);
    else
        return new_set(base_insert((-i - 1), x, set->elems));
}

doremir_set_t doremir_set_remove(doremir_ptr_t x, doremir_set_t set)
{
    int i = base_index_of(x, set->elems);
    if (i < 0)
        return doremir_set_copy(set);
    else
        return new_set(base_remove(i, set->elems));
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

int doremir_set_size(doremir_set_t set)
{
    return base_length(set->elems);
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
    base_for_each (a->elems, is_last, x)
    {
        if (!doremir_set_has(x, b))
            return false;
    }
    return true;
}

bool doremir_set_is_proper_subset_of(doremir_set_t a, doremir_set_t b)
{
    base_for_each (a->elems, is_last, x)
    {
        if (!doremir_set_has(x, b))
            return false;
    }
    return doremir_set_size(a) != doremir_set_size(b);
}

doremir_set_t doremir_set_sum(doremir_set_t a, doremir_set_t b)
{
    set_t c = a;
    base_for_each (b->elems, _, x)
    {
        c = doremir_set_add(x, c);
    }
    return c;
}

doremir_set_t doremir_set_difference(doremir_set_t a, doremir_set_t b)
{
    set_t c = a;
    base_for_each (b->elems, _, x)
    {
        c = doremir_set_remove(x, c);
    }
    return c;
}

doremir_set_t doremir_set_product(doremir_set_t a, doremir_set_t b)
{
    set_t c = doremir_set_empty();
    base_for_each (a->elems, is_last, x)
    {
        base_for_each (b->elems, is_last, y)
        {
            c = doremir_set_add(pair(x, y), c);
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
    for (int i = 0; i < count; ++i)
        s = doremir_set_add(va_arg(args, ptr_t), s);

    va_end(args);
    return s;
}

// --------------------------------------------------------------------------------

bool set_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return base_equal(c->elems, d->elems);
}

bool set_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return base_less_than(c->elems, d->elems);
}

bool set_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    set_t c = (set_t) a;
    set_t d = (set_t) b;
    return base_greater_than(c->elems, d->elems);
}

doremir_string_t set_show(doremir_ptr_t x)
{
    set_t set = (set_t) x;
    string_t s  = string("{");

    base_for_each (set->elems, is_last, v)
    {
        s = string_dappend(s, doremir_string_show(v));
        if (!is_last)
            s = string_dappend(s, string(","));
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

doremir_ptr_t set_impl(doremir_id_t interface)
{
    static doremir_equal_t set_equal_impl = { set_equal };
    static doremir_string_show_t set_show_impl = { set_show };
    static doremir_copy_t set_copy_impl = { set_copy };
    static doremir_destroy_t set_destroy_impl = { set_destroy };

    switch (interface)
    {
    case doremir_equal_i:
        return &set_equal_impl;

    case doremir_string_show_i:
        return &set_show_impl;

    case doremir_copy_i:
        return &set_copy_impl;

    case doremir_destroy_i:
        return &set_destroy_impl;

    default:
        return NULL;
    }
}

