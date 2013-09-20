
/*
    FA
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/pair.h>
#include <fa/dynamic.h>
#include <fa/util.h>

struct _fa_pair_t {
    impl_t      impl;       //  Interface dispatcher

    ptr_t       first;        //  Values
    ptr_t       second;
};

// -----------------------------------------------------------------------------

pair_t new_pair(fa_ptr_t first, fa_ptr_t second)
{
    fa_ptr_t pair_impl(fa_id_t interface);

    pair_t pair = fa_new(pair);
    pair->impl = &pair_impl;
    pair->first  = first;
    pair->second  = second;
    return pair;
}

void delete_pair(pair_t p)
{
    fa_delete(p);
}


// -----------------------------------------------------------------------------

/** Create a new pair.
 */
fa_pair_t fa_pair_create(fa_ptr_t first, fa_ptr_t second)
{
    return new_pair(first, second);
}

/** Create a pair by reading the components of a structure.
 */
fa_pair_t fa_pair_read(fa_pair_struct_t *input)
{
    return new_pair(input->first, input->second);
}

/** Write the values of a pair to a structure.
 */
void fa_pair_write(fa_pair_struct_t *output, fa_pair_t pair)
{
    output->first = pair->first;
    output->second = pair->second;
}

/** Copy the given pair.
 */
fa_pair_t fa_pair_copy(fa_pair_t pair)
{
    return new_pair(pair->first, pair->second);
}

/** Destroy the given pair.
 */
void fa_pair_destroy(fa_pair_t pair)
{
    delete_pair(pair);
}

/** Get the first component of the given pair.
 */
fa_ptr_t fa_pair_first(fa_pair_t pair)
{
    return pair->first;
}

/** Get the second component of the given pair.
 */
fa_ptr_t fa_pair_second(fa_pair_t pair)
{
    return pair->second;
}

/** Return a pair containing the given value as both its left and right component.
 */
fa_pair_t fa_pair_duplicate(fa_ptr_t value)
{
    return new_pair(fa_copy(value), fa_copy(value));
}

/** Swap the components of the given pair.
 */
fa_pair_t fa_pair_swap(fa_pair_t pair)
{
    return new_pair(fa_copy(pair->second), fa_copy(pair->first));
}

// (a, (b, c)) -> ((a, b), c)
/** Return the left-associated version of the given nested pair.
 */
fa_pair_t fa_pair_assoc(fa_pair_t p)
{
    ptr_t a = fa_copy(p->first);
    ptr_t b = fa_copy(((pair_t) p->second)->first);
    ptr_t c = fa_copy(((pair_t) p->second)->second);

    return new_pair(new_pair(a, b), c);
}

// ((a, b), c) -> (a, (b, c))
/** Return the right-associated version of the given nested pair.
 */
fa_pair_t fa_pair_unassoc(fa_pair_t p)
{
    ptr_t a = fa_copy(((pair_t) p->first)->first);
    ptr_t b = fa_copy(((pair_t) p->first)->second);
    ptr_t c = fa_copy(p->second);
    return new_pair(a, new_pair(b, c));
}


// --------------------------------------------------------------------------------

fa_ptr_t fa_pair_dfirst(fa_pair_t pair)
{
    ptr_t value = pair->first;
    fa_destroy(pair);
    return value;
}

fa_ptr_t fa_pair_dsecond(fa_pair_t pair)
{
    ptr_t value = pair->second;
    fa_destroy(pair);
    return value;
}

fa_pair_t fa_pair_dduplicate(fa_ptr_t value)
{
    return new_pair(value, fa_copy(value));
}

fa_pair_t fa_pair_dswap(fa_pair_t pair)
{
    pair_t pair2 = new_pair(pair->second, pair->first);
    fa_destroy(pair);
    return pair2;
}

// (a, (b, c)) -> ((a, b), c)
fa_pair_t fa_pair_dassoc(fa_pair_t pair)
{
    pair_t pair2 = fa_pair_assoc(pair);
    fa_destroy(pair->second);
    fa_destroy(pair);
    return pair2;
}

// ((a, b), c) -> (a, (b, c))
fa_pair_t fa_pair_dunassoc(fa_pair_t pair)
{
    pair_t pair2 = fa_pair_unassoc(pair);
    fa_destroy(pair->first);
    fa_destroy(pair);
    return pair2;
}

fa_list_t fa_pair_to_list(fa_pair_t pair)
{
    return list(pair->first, pair->second);
}

bool pair_equal(fa_ptr_t a, fa_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fa_equal(c->first, d->first) && fa_equal(c->second, d->second);
}

bool pair_less_than(fa_ptr_t a, fa_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fa_less_than(c->first, d->first) || (fa_equal(c->first, d->first) && fa_less_than(c->second, d->second));
}

bool pair_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fa_greater_than(c->first, d->first) || (fa_equal(c->first, d->first) && fa_greater_than(c->second, d->second));
}

fa_string_t pair_show(fa_ptr_t a)
{
    pair_t b = (pair_t) a;
    string_t s = string("(");
    s = string_dappend(s, fa_string_show(b->first));
    s = string_dappend(s, string(","));
    s = string_dappend(s, fa_string_show(b->second));
    s = string_dappend(s, string(")"));
    return s;
}

fa_ptr_t pair_copy(fa_ptr_t a)
{
    return fa_pair_copy(a);
}

void pair_destroy(fa_ptr_t a)
{
    fa_pair_destroy(a);
}

type_repr_t pair_get_type(fa_ptr_t a)
{
    return pair_type_repr;
}

fa_ptr_t pair_impl(fa_id_t interface)
{
    static fa_equal_t pair_equal_impl = { pair_equal };
    static fa_order_t pair_order_impl = { pair_less_than, pair_greater_than };
    static fa_string_show_t pair_show_impl = { pair_show };
    static fa_copy_t pair_copy_impl = { pair_copy };
    static fa_destroy_t pair_destroy_impl = { pair_destroy };
    static fa_dynamic_t pair_dynamic_impl = { pair_get_type };

    switch (interface) {
    case fa_equal_i:
        return &pair_equal_impl;

    case fa_order_i:
        return &pair_order_impl;

    case fa_string_show_i:
        return &pair_show_impl;

    case fa_copy_i:
        return &pair_copy_impl;

    case fa_destroy_i:
        return &pair_destroy_impl;

    case fa_dynamic_i:
        return &pair_dynamic_impl;

    default:
        return NULL;
    }
}

