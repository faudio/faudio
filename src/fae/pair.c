
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

    ptr_t       fst;        //  Values
    ptr_t       snd;
};

// -----------------------------------------------------------------------------

pair_t new_pair(fae_ptr_t fst, fae_ptr_t snd)
{
    fae_ptr_t pair_impl(fae_id_t interface);

    pair_t pair = fae_new(pair);
    pair->impl = &pair_impl;
    pair->fst  = fst;
    pair->snd  = snd;
    return pair;
}

void delete_pair(pair_t p)
{
    fae_delete(p);
}


// -----------------------------------------------------------------------------

/** Create a new pair.
 */
fae_pair_t fae_pair_create(fae_ptr_t fst, fae_ptr_t snd)
{
    return new_pair(fst, snd);
}

/** Copy the given pair.
 */
fae_pair_t fae_pair_copy(fae_pair_t pair)
{
    return new_pair(pair->fst, pair->snd);
}

/** Destroy the given pair.
 */
void fae_pair_destroy(fae_pair_t pair)
{
    delete_pair(pair);
}

/** Get the first component of the given pair.
 */
fae_ptr_t fae_pair_fst(fae_pair_t pair)
{
    return pair->fst;
}

/** Get the second component of the given pair.
 */
fae_ptr_t fae_pair_snd(fae_pair_t pair)
{
    return pair->snd;
}

/** Return a pair containing the given value as both its left and right component.
 */
fae_pair_t fae_pair_dup(fae_ptr_t value)
{
    return new_pair(fae_copy(value), fae_copy(value));
}

/** Swap the components of the given pair.
 */
fae_pair_t fae_pair_swap(fae_pair_t pair)
{
    return new_pair(fae_copy(pair->snd), fae_copy(pair->fst));
}

// (a, (b, c)) -> ((a, b), c)
/** Return the left-associated version of the given nested pair.
 */
fae_pair_t fae_pair_assoc(fae_pair_t p)
{
    ptr_t a = fae_copy(p->fst);
    ptr_t b = fae_copy(((pair_t) p->snd)->fst);
    ptr_t c = fae_copy(((pair_t) p->snd)->snd);

    return new_pair(new_pair(a, b), c);
}

// ((a, b), c) -> (a, (b, c))
/** Return the right-associated version of the given nested pair.
 */
fae_pair_t fae_pair_unassoc(fae_pair_t p)
{
    ptr_t a = fae_copy(((pair_t) p->fst)->fst);
    ptr_t b = fae_copy(((pair_t) p->fst)->snd);
    ptr_t c = fae_copy(p->snd);
    return new_pair(a, new_pair(b, c));
}


// --------------------------------------------------------------------------------

fae_ptr_t fae_pair_dfst(fae_pair_t pair)
{
    ptr_t value = pair->fst;
    fae_destroy(pair);
    return value;
}

fae_ptr_t fae_pair_dsnd(fae_pair_t pair)
{
    ptr_t value = pair->snd;
    fae_destroy(pair);
    return value;
}

fae_pair_t fae_pair_ddup(fae_ptr_t value)
{
    return new_pair(value, fae_copy(value));
}

fae_pair_t fae_pair_dswap(fae_pair_t pair)
{
    pair_t pair2 = new_pair(pair->snd, pair->fst);
    fae_destroy(pair);
    return pair2;
}

// (a, (b, c)) -> ((a, b), c)
fae_pair_t fae_pair_dassoc(fae_pair_t pair)
{
    pair_t pair2 = fae_pair_assoc(pair);
    fae_destroy(pair->snd);
    fae_destroy(pair);
    return pair2;
}

// ((a, b), c) -> (a, (b, c))
fae_pair_t fae_pair_dunassoc(fae_pair_t pair)
{
    pair_t pair2 = fae_pair_unassoc(pair);
    fae_destroy(pair->fst);
    fae_destroy(pair);
    return pair2;
}

fae_list_t fae_pair_to_list(fae_pair_t pair)
{
    return list(pair->fst, pair->snd);
}

bool pair_equal(fae_ptr_t a, fae_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fae_equal(c->fst, d->fst) && fae_equal(c->snd, d->snd);
}

bool pair_less_than(fae_ptr_t a, fae_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fae_less_than(c->fst, d->fst) || (fae_equal(c->fst, d->fst) && fae_less_than(c->snd, d->snd));
}

bool pair_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return fae_greater_than(c->fst, d->fst) || (fae_equal(c->fst, d->fst) && fae_greater_than(c->snd, d->snd));
}

fae_string_t pair_show(fae_ptr_t a)
{
    pair_t b = (pair_t) a;
    string_t s = string("(");
    s = string_dappend(s, fae_string_show(b->fst));
    s = string_dappend(s, string(","));
    s = string_dappend(s, fae_string_show(b->snd));
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

