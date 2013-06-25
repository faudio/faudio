
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/pair.h>
#include <fae/dynamic.h>
#include <fae/util.h>

struct _fae_pair_t {
    impl_t      impl;       //  Interface dispatcher

    ptr_t       first;        //  Values
    ptr_t       second;
};

// -----------------------------------------------------------------------------

pair_t new_pair(fae_ptr_t first, fae_ptr_t second)
{
    fae_ptr_t pair_impl(fae_id_t interface);

    pair_t pair = fae_new(pair);
    pair->impl = &pair_impl;
    pair->first  = first;
    pair->second  = second;
    return pair;
}

void delete_pair(pair_t p)
{
    fae_delete(p);
}


// -----------------------------------------------------------------------------

/** Create a new pair.
 */
fae_pair_t fae_pair_create(fae_ptr_t first, fae_ptr_t second)
{
    return new_pair(first, second);
}

/** Create a pair by reading the components of a structure.
 */
fae_pair_t fae_pair_read(fae_pair_struct_t *input)
{
    return new_pair(input->first, input->second);
}

/** Write the values of a pair to a structure.
 */
void fae_pair_write(fae_pair_struct_t *output, fae_pair_t pair)
{
    output->first = pair->first;
    output->second = pair->second;
}

/** Copy the given pair.
 */
fae_pair_t fae_pair_copy(fae_pair_t pair)
{
    return new_pair(pair->first, pair->second);
}

/** Destroy the given pair.
 */
void fae_pair_destroy(fae_pair_t pair)
{
    delete_pair(pair);
}

/** Get the first component of the given pair.
 */
fae_ptr_t fae_pair_first(fae_pair_t pair)
{
    return pair->first;
}

/** Get the second component of the given pair.
 */
fae_ptr_t fae_pair_second(fae_pair_t pair)
{
    return pair->second;
}

/** Return a pair containing the given value as both its left and right component.
 */
fae_pair_t fae_pair_duplicate(fae_ptr_t value)
{
    return new_pair(fae_copy(value), fae_copy(value));
}

/** Swap the components of the given pair.
 */
fae_pair_t fae_pair_swap(fae_pair_t pair)
{
    return new_pair(fae_copy(pair->second), fae_copy(pair->first));
}

// (a, (b, c)) -> ((a, b), c)
/** Return the left-associated version of the given nested pair.
 */
fae_pair_t fae_pair_assoc(fae_pair_t p)
{
    ptr_t a = fae_copy(p->first);
    ptr_t b = fae_copy(((pair_t) p->second)->first);
    ptr_t c = fae_copy(((pair_t) p->second)->second);

    return new_pair(new_pair(a, b), c);
}

// ((a, b), c) -> (a, (b, c))
/** Return the right-associated version of the given nested pair.
 */
fae_pair_t fae_pair_unassoc(fae_pair_t p)
{
    ptr_t a = fae_copy(((pair_t) p->first)->first);
    ptr_t b = fae_copy(((pair_t) p->first)->second);
    ptr_t c = fae_copy(p->second);
    return new_pair(a, new_pair(b, c));
}


// --------------------------------------------------------------------------------

fae_ptr_t fae_pair_dfirst(fae_pair_t pair)
{
    ptr_t value = pair->first;
    fae_destroy(pair);
    return value;
}

fae_ptr_t fae_pair_dsecond(fae_pair_t pair)
{
    ptr_t value = pair->second;
    fae_destroy(pair);
    return value;
}

fae_pair_t fae_pair_dduplicate(fae_ptr_t value)
{
    return new_pair(value, fae_copy(value));
}

fae_pair_t fae_pair_dswap(fae_pair_t pair)
{
    pair_t pair2 = new_pair(pair->second, pair->first);
    fae_destroy(pair);
    return pair2;
}

// (a, (b, c)) -> ((a, b), c)
fae_pair_t fae_pair_dassoc(fae_pair_t pair)
{
    pair_t pair2 = fae_pair_assoc(pair);
    fae_destroy(pair->second);
    fae_destroy(pair);
    return pair2;
}

// ((a, b), c) -> (a, (b, c))
fae_pair_t fae_pair_dunassoc(fae_pair_t pair)
{
    pair_t pair2 = fae_pair_unassoc(pair);
    fae_destroy(pair->first);
    fae_destroy(pair);
    return pair2;
}

fae_list_t fae_pair_to_list(fae_pair_t pair)
{
    return list(pair->first, pair->second);
}

bool pair_equal(fae_ptr_t a, fae_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fae_equal(c->first, d->first) && fae_equal(c->second, d->second);
}

bool pair_less_than(fae_ptr_t a, fae_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fae_less_than(c->first, d->first) || (fae_equal(c->first, d->first) && fae_less_than(c->second, d->second));
}

bool pair_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fae_greater_than(c->first, d->first) || (fae_equal(c->first, d->first) && fae_greater_than(c->second, d->second));
}

fae_string_t pair_show(fae_ptr_t a)
{
    pair_t b = (pair_t) a;
    string_t s = string("(");
    s = string_dappend(s, fae_string_show(b->first));
    s = string_dappend(s, string(","));
    s = string_dappend(s, fae_string_show(b->second));
    s = string_dappend(s, string(")"));
    return s;
}

fae_ptr_t pair_copy(fae_ptr_t a)
{
    return fae_pair_copy(a);
}

void pair_destroy(fae_ptr_t a)
{
    fae_pair_destroy(a);
}

type_repr_t pair_get_type(fae_ptr_t a)
{
    return pair_type_repr;
}

fae_ptr_t pair_impl(fae_id_t interface)
{
    static fae_equal_t pair_equal_impl = { pair_equal };
    static fae_order_t pair_order_impl = { pair_less_than, pair_greater_than };
    static fae_string_show_t pair_show_impl = { pair_show };
    static fae_copy_t pair_copy_impl = { pair_copy };
    static fae_destroy_t pair_destroy_impl = { pair_destroy };
    static fae_dynamic_t pair_dynamic_impl = { pair_get_type };

    switch (interface) {
    case fae_equal_i:
        return &pair_equal_impl;

    case fae_order_i:
        return &pair_order_impl;

    case fae_string_show_i:
        return &pair_show_impl;

    case fae_copy_i:
        return &pair_copy_impl;

    case fae_destroy_i:
        return &pair_destroy_impl;

    case fae_dynamic_i:
        return &pair_dynamic_impl;

    default:
        return NULL;
    }
}

