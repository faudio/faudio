
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/set.h>
#include <doremir/set.h>
#include <doremir/pair.h>
#include <doremir/util.h>

/*  Naive set implementation based on lists.

    Can be optimized *a lot* by using a persistent vector/int-map as the underlying collection.
 */


struct _doremir_set_t {
        doremir_impl_t  impl;       /* Interface dispatcher */
        list_t          elems;
    };

doremir_ptr_t
set_impl(doremir_id_t interface);

inline static set_t
new_set(list_t elems)
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
    return new_set(doremir_list_empty());
}

doremir_set_t doremir_set_add(doremir_ptr_t x, doremir_set_t set)
{
    int i = doremir_list_find_index(x, set->elems);
    if (i >= 0)
        return doremir_set_copy(set);
    else
        return new_set(doremir_list_insert(-i, x, set->elems));
}

doremir_set_t doremir_set_remove(doremir_ptr_t x, doremir_set_t set)
{
    int i = doremir_list_find_index(x, set->elems);
    if (i < 0)
        return doremir_set_copy(set);
    else
        return new_set(doremir_list_remove_range(i, 1, set->elems));
}

doremir_set_t doremir_set_copy(doremir_set_t set)
{
    return new_set(doremir_list_copy(set->elems));
}

void doremir_set_destroy(doremir_set_t set)
{
    doremir_list_destroy(set->elems);
    delete_set(set);
}

// --------------------------------------------------------------------------------

bool doremir_set_has(doremir_ptr_t x, doremir_set_t set)
{
    return doremir_list_find_index(x, set->elems) >= 0;
}

int doremir_set_size(doremir_set_t set)
{
    return doremir_list_length(set->elems);
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
    assert(false && "Not implemented");
}

bool doremir_set_is_proper_subset_of(doremir_set_t a, doremir_set_t b)
{
    assert(false && "Not implemented");
}

doremir_set_t doremir_set_sum(doremir_set_t a, doremir_set_t b)
{
    assert(false && "Not implemented");
}

doremir_set_t doremir_set_product(doremir_set_t a, doremir_set_t b)
{
    assert(false && "Not implemented");
}

doremir_set_t doremir_set_difference(doremir_set_t a, doremir_set_t b)
{
    assert(false && "Not implemented");
}

doremir_set_t doremir_set_cartesian(doremir_set_t a, doremir_set_t b)
{
    assert(false && "Not implemented");
}

doremir_set_t doremir_set_power(doremir_set_t set)
{
    assert(false && "Not implemented");
}



// --------------------------------------------------------------------------------

// bool set_equal(doremir_ptr_t a, doremir_ptr_t b)
// {
//     node_t an = ((set_t) a)->node;
//     node_t bn = ((set_t) b)->node;
//     while (an && bn)
//     {
//         if (!eq(an->value, bn->value))
//             return false;
//         an = an->next;
//         bn = bn->next;
//     }
//     return !(an || bn);
// }
//
// bool set_less_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//     node_t an = ((set_t) a)->node;
//     node_t bn = ((set_t) b)->node;
//     while (an && bn)
//     {
//         if (lt(an->value, bn->value))
//             return true;
//         if (gt(an->value, bn->value))
//             return false;
//         an = an->next;
//         bn = bn->next;
//     }
//     return bn && !an;
// }
//
// bool set_greater_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//     node_t an = ((set_t) a)->node;
//     node_t bn = ((set_t) b)->node;
//     while (an && bn)
//     {
//         if (gt(an->value, bn->value))
//             return true;
//         if (lt(an->value, bn->value))
//             return false;
//         an = an->next;
//         bn = bn->next;
//     }
//     return an && !bn;
// }
//
// doremir_string_t set_show(doremir_ptr_t xs)
// {
//     string_t s  = string("[");
//     node_t   xn = ((set_t) xs)->node;
//     while(xn)
//     {
//         sap(s, sshow(xn->value));
//         xn = xn->next;
//         if (xn)
//             sap(s, string(","));
//     };
//     sap(s, string("]"));
//     return s;
// }
//
// doremir_ptr_t set_copy(doremir_ptr_t a)
// {
//     return doremir_set_copy(a);
// }
//
// void set_destroy(doremir_ptr_t a)
// {
//     doremir_set_destroy(a);
// }

doremir_ptr_t set_impl(doremir_id_t interface)
{
    // static doremir_equal_t set_equal_impl = { set_equal };
    // static doremir_string_show_t set_show_impl = { set_show };
    // static doremir_copy_t set_copy_impl = { set_copy };
    // static doremir_destroy_t set_destroy_impl = { set_destroy };
    //
    // switch (interface)
    // {
    // case doremir_equal_i:
    //     return &set_equal_impl;
    //
    // case doremir_string_show_i:
    //     return &set_show_impl;
    //
    // case doremir_copy_i:
    //     return &set_copy_impl;
    //
    // case doremir_destroy_i:
    //     return &set_destroy_impl;
    //
    // default:
    //     return NULL;
    // }
}

