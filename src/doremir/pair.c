
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/pair.h>
#include <doremir/util.h>

struct _doremir_pair_t {
        doremir_impl_t  impl;       /* Interface dispatcher */
        doremir_ptr_t   fst;
        doremir_ptr_t   snd;
};

doremir_ptr_t pair_impl(doremir_id_t interface);

pair_t new_pair()
{
    pair_t p = doremir_new(pair);
    p->impl = &pair_impl;
    return p;
}
void delete_pair(pair_t p)
{
    doremir_delete(p);
}

doremir_pair_t doremir_pair_create(doremir_ptr_t fst, doremir_ptr_t snd)
{
    pair_t p = new_pair();
    p->fst  = fst;
    p->snd  = snd;
    return p;
}

doremir_pair_t doremir_pair_copy(doremir_pair_t p)
{
    pair_t q = new_pair();
    q->fst = p->fst;
    q->snd = p->snd;
    return q;
}

void doremir_pair_destroy(doremir_pair_t p)
{
    delete_pair(p);
}

// --------------------------------------------------------------------------------

bool pair_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return eq(c->fst, d->fst) && eq(c->snd, d->snd);
}

bool pair_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return lt(c->fst, d->fst) || (eq(c->fst, d->fst) && lt(c->snd, d->snd));
}

bool pair_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{          
    pair_t c = (pair_t) a;
    pair_t d = (pair_t) b;
    return gt(c->fst, d->fst) || (eq(c->fst, d->fst) && gt(c->snd, d->snd));
}

doremir_string_t pair_show(doremir_ptr_t a)
{
    pair_t b = (pair_t) a;
    string_t s = string("(");
    s = sdappend(s, sshow(b->fst));
    s = sdappend(s, string(","));
    s = sdappend(s, sshow(b->snd));
    s = sdappend(s, string(")"));
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

    switch (interface)
    {
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