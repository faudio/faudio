
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/pair.h>
#include <fa/pair/left.h>
#include <fa/util.h>

struct _fa_pair_left_t {
    fa_impl_t      impl;
    fa_ptr_t       values[2];
};

static int gPairLeftCount = 0;

// -----------------------------------------------------------------------------

fa_pair_left_t new_pair_left(fa_ptr_t first, fa_ptr_t second)
{
    fa_ptr_t pair_left_impl(fa_id_t interface);

    fa_pair_left_t pair_left = fa_new(pair_left);
    pair_left->impl = &pair_left_impl;
    pair_left->values[0]  = first;
    pair_left->values[1]  = second;
    gPairLeftCount++;
    return pair_left;
}

void delete_pair_left(fa_pair_left_t p)
{
    gPairLeftCount--;
    fa_delete(p);
}


// -----------------------------------------------------------------------------

fa_pair_left_t fa_pair_left_create(fa_ptr_t first, fa_ptr_t second)
{
    return new_pair_left(first, second);
}

fa_pair_left_t fa_pair_left_read(fa_pair_left_struct_t *input)
{
    return new_pair_left(input->first, input->second);
}

void fa_pair_left_write(fa_pair_left_struct_t *output, fa_pair_left_t pair_left)
{
    output->first  = pair_left->values[0];
    output->second = pair_left->values[1];
}

void fa_pair_left_decons(fa_ptr_t *a, fa_ptr_t *b, fa_pair_left_t pair_left)
{
    *a = pair_left->values[0];
    *b = pair_left->values[1];
}

fa_pair_left_t fa_pair_left_copy(fa_pair_left_t pair_left)
{
    return new_pair_left(pair_left->values[0], pair_left->values[1]);
}

fa_pair_left_t fa_pair_left_deep_copy(fa_pair_left_t pair_left)
{
    return new_pair_left(fa_deep_copy(pair_left->values[0]), fa_deep_copy(pair_left->values[1]));
}

void fa_pair_left_destroy(fa_pair_left_t pair_left)
{
    delete_pair_left(pair_left);
}

void fa_pair_left_deep_destroy(fa_pair_left_t pair_left, fa_deep_destroy_pred_t pred)
{
    if (!(pred(pair_left))) return;
    fa_ptr_t a = pair_left->values[0];
    fa_ptr_t b = pair_left->values[1];
    if (a) fa_deep_destroy(a, pred);
    if (b) fa_deep_destroy(b, pred);
    delete_pair_left(pair_left);
}

fa_pair_t fa_pair_left_to_pair(fa_pair_left_t pair_left)
{
    return fa_pair_create(pair_left->values[0], pair_left->values[1]);
}

fa_pair_left_t fa_pair_left_from_pair(fa_pair_t pair)
{
    return fa_pair_left_create(fa_pair_first(pair), fa_pair_second(pair));
}

fa_list_t fa_pair_left_to_list(fa_pair_left_t pair_left)
{
    return list(pair_left->values[0], pair_left->values[1]);
}



// --------------------------------------------------------------------------------

bool pair_left_equal(fa_ptr_t a, fa_ptr_t b)
{
    fa_pair_left_t c = (fa_pair_left_t) a;
    fa_pair_left_t d = (fa_pair_left_t) b;
    return fa_equal(c->values[0], d->values[0]);
}

bool pair_left_less_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_pair_left_t c = (fa_pair_left_t) a;
    fa_pair_left_t d = (fa_pair_left_t) b;
    return fa_less_than(c->values[0], d->values[0]);
}

bool pair_left_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_pair_left_t c = (fa_pair_left_t) a;
    fa_pair_left_t d = (fa_pair_left_t) b;
    return fa_greater_than(c->values[0], d->values[0]);
}

fa_string_t pair_left_show(fa_ptr_t a)
{
    fa_pair_left_t b = (fa_pair_left_t) a;
    fa_string_t s = fa_string("~(");
    s = fa_string_dappend(s, fa_string_show(b->values[0]));
    s = fa_string_dappend(s, fa_string(","));
    s = fa_string_dappend(s, fa_string_show(b->values[1]));
    s = fa_string_dappend(s, fa_string(")"));
    return s;
}

fa_ptr_t pair_left_copy(fa_ptr_t a)
{
    return fa_pair_left_copy(a);
}

fa_ptr_t pair_left_deep_copy(fa_ptr_t a)
{
    return fa_pair_left_deep_copy(a);
}

void pair_left_destroy(fa_ptr_t a)
{
    fa_pair_left_destroy(a);
}

void pair_left_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    fa_pair_left_deep_destroy(a, p);
}

fa_ptr_t pair_left_impl(fa_id_t interface)
{
    static fa_equal_t pair_left_equal_impl = { pair_left_equal };
    static fa_order_t pair_left_order_impl = { pair_left_less_than, pair_left_greater_than };
    static fa_string_show_t pair_left_show_impl = { pair_left_show };
    static fa_copy_t pair_left_copy_impl = { pair_left_copy, pair_left_deep_copy };
    static fa_destroy_t pair_left_destroy_impl = { pair_left_destroy, pair_left_deep_destroy };

    switch (interface) {
    case fa_equal_i:
        return &pair_left_equal_impl;

    case fa_order_i:
        return &pair_left_order_impl;

    case fa_string_show_i:
        return &pair_left_show_impl;

    case fa_copy_i:
        return &pair_left_copy_impl;

    case fa_destroy_i:
        return &pair_left_destroy_impl;

    default:
        return NULL;
    }
}

void fa_pair_left_log_count()
{
  fa_log_info(fa_string_dappend(fa_string("Pair-lefts allocated: "), fa_string_dshow(fa_i32(gPairLeftCount))));
}
