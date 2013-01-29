
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/pair.h>
#include <doremir/util.h>

struct _doremir_pair_t {
    impl_t      impl;       //  Interface dispatcher

    ptr_t       fst;        //  Values
    ptr_t       snd;
};

// -----------------------------------------------------------------------------

pair_t new_pair(doremir_ptr_t fst, doremir_ptr_t snd)
{
    doremir_ptr_t pair_impl(doremir_id_t interface);

    pair_t pair = doremir_new(pair);
    pair->impl = &pair_impl;
    pair->fst  = fst;
    pair->snd  = snd;
    return pair;
}
void delete_pair(pair_t p)
{
    doremir_delete(p);
}

doremir_pair_t doremir_pair_create(doremir_ptr_t fst, doremir_ptr_t snd)
{
    return new_pair(fst, snd);
}

doremir_pair_t doremir_pair_copy(doremir_pair_t pair)
{
    return new_pair(pair->fst, pair->snd);
}

void doremir_pair_destroy(doremir_pair_t pair)
{
    delete_pair(pair);
}

// --------------------------------------------------------------------------------

doremir_ptr_t doremir_pair_fst(doremir_pair_t pair)
{
    return pair->fst;
}

doremir_ptr_t doremir_pair_snd(doremir_pair_t pair)
{
    return pair->snd;
}

doremir_pair_t doremir_pair_dup(doremir_ptr_t value)
{
    return new_pair(doremir_copy(value), doremir_copy(value));
}

doremir_pair_t doremir_pair_swap(doremir_pair_t pair)
{
    return new_pair(doremir_copy(pair->snd), doremir_copy(pair->fst));
}

// (a, (b, c)) -> ((a, b), c)
doremir_pair_t doremir_pair_assoc(doremir_pair_t p)
{
    ptr_t a = doremir_copy(p->fst);
    ptr_t b = doremir_copy(((pair_t) p->snd)->fst);
    ptr_t c = doremir_copy(((pair_t) p->snd)->snd);

    return new_pair(new_pair(a, b), c);
}

// ((a, b), c) -> (a, (b, c))
doremir_pair_t doremir_pair_unassoc(doremir_pair_t p)
{
    ptr_t a = doremir_copy(((pair_t) p->fst)->fst);
    ptr_t b = doremir_copy(((pair_t) p->fst)->snd);
    ptr_t c = doremir_copy(p->snd);
    return new_pair(a, new_pair(b, c));
}


// --------------------------------------------------------------------------------

doremir_ptr_t doremir_pair_dfst(doremir_pair_t pair)
{
    ptr_t value = pair->fst;
    doremir_destroy(pair);
    return value;
}

doremir_ptr_t doremir_pair_dsnd(doremir_pair_t pair)
{
    ptr_t value = pair->snd;
    doremir_destroy(pair);
    return value;
}

doremir_pair_t doremir_pair_ddup(doremir_ptr_t value)
{
    return new_pair(value, doremir_copy(value));
}

doremir_pair_t doremir_pair_dswap(doremir_pair_t pair)
{
    pair_t pair2 = new_pair(pair->snd, pair->fst);
    doremir_destroy(pair);
    return pair2;
}

// (a, (b, c)) -> ((a, b), c)
doremir_pair_t doremir_pair_dassoc(doremir_pair_t pair)
{
    pair_t pair2 = doremir_pair_assoc(pair);
    doremir_destroy(pair->snd);
    doremir_destroy(pair);
    return pair2;
}

// ((a, b), c) -> (a, (b, c))
doremir_pair_t doremir_pair_dunassoc(doremir_pair_t pair)
{
    pair_t pair2 = doremir_pair_unassoc(pair);
    doremir_destroy(pair->fst);
    doremir_destroy(pair);
    return pair2;
}


// --------------------------------------------------------------------------------

bool pair_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return doremir_equal(c->fst, d->fst) && doremir_equal(c->snd, d->snd);
}

bool pair_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return doremir_less_than(c->fst, d->fst) || (doremir_equal(c->fst, d->fst) && doremir_less_than(c->snd, d->snd));
}

bool pair_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return doremir_greater_than(c->fst, d->fst) || (doremir_equal(c->fst, d->fst) && doremir_greater_than(c->snd, d->snd));
}

doremir_string_t pair_show(doremir_ptr_t a)
{
    pair_t b = (pair_t) a;
    string_t s = string("(");
    s = string_dappend(s, doremir_string_show(b->fst));
    s = string_dappend(s, string(","));
    s = string_dappend(s, doremir_string_show(b->snd));
    s = string_dappend(s, string(")"));
    return s;
}

doremir_ptr_t pair_copy(doremir_ptr_t a)
{
    return doremir_pair_copy(a);
}

void pair_destroy(doremir_ptr_t a)
{
    doremir_pair_destroy(a);
}

doremir_ptr_t pair_impl(doremir_id_t interface)
{
    static doremir_equal_t pair_equal_impl = { pair_equal };
    static doremir_order_t pair_order_impl = { pair_less_than, pair_greater_than };
    static doremir_string_show_t pair_show_impl = { pair_show };
    static doremir_copy_t pair_copy_impl = { pair_copy };
    static doremir_destroy_t pair_destroy_impl = { pair_destroy };

    switch (interface) {
        case doremir_equal_i:
            return &pair_equal_impl;

        case doremir_order_i:
            return &pair_order_impl;

        case doremir_string_show_i:
            return &pair_show_impl;

        case doremir_copy_i:
            return &pair_copy_impl;

        case doremir_destroy_i:
            return &pair_destroy_impl;

        default:
            return NULL;
    }
}

