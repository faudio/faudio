
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2015
    All rights reserved.

 */

#include <fa/pair.h>
#include <fa/dynamic.h>
#include <fa/util.h>

struct _fa_pair_t {
    fa_impl_t      impl;
    fa_ptr_t       values[2];
};

static int gPairCount = 0;

// -----------------------------------------------------------------------------

fa_pair_t new_pair(fa_ptr_t first, fa_ptr_t second)
{
    fa_ptr_t pair_impl(fa_id_t interface);

    fa_pair_t pair = fa_new(pair);
    pair->impl = &pair_impl;
    pair->values[0]  = first;
    pair->values[1]  = second;
    gPairCount++;
    return pair;
}

void delete_pair(fa_pair_t p)
{
    gPairCount--;
    fa_delete(p);
}


// -----------------------------------------------------------------------------

fa_pair_t fa_pair_create(fa_ptr_t first, fa_ptr_t second)
{
    return new_pair(first, second);
}

fa_pair_t fa_pair_read(fa_pair_struct_t *input)
{
    return new_pair(input->first, input->second);
}

void fa_pair_write(fa_pair_struct_t *output, fa_pair_t pair)
{
    output->first  = pair->values[0];
    output->second = pair->values[1];
}

void fa_pair_decons(fa_ptr_t *a, fa_ptr_t *b, fa_pair_t pair)
{
    *a = pair->values[0];
    *b = pair->values[1];
}

fa_pair_t fa_pair_copy(fa_pair_t pair)
{
    return new_pair(pair->values[0], pair->values[1]);
}

fa_pair_t fa_pair_deep_copy(fa_pair_t pair)
{
    return new_pair(fa_deep_copy(pair->values[0]), fa_deep_copy(pair->values[1]));
}

void fa_pair_destroy(fa_pair_t pair)
{
    delete_pair(pair);
}

void fa_pair_deep_destroy(fa_pair_t pair, fa_deep_destroy_pred_t pred)
{
    if (!pred(pair)) return;
    fa_ptr_t a = pair->values[0];
    fa_ptr_t b = pair->values[1];
    //fa_log_info(fa_dappend(fa_string("  destroying pair 1 "), fa_string_show(pair)));
    if (a) fa_deep_destroy(a, pred);
    //fa_log_info(fa_string("  destroying pair 2"));
    if (b) fa_deep_destroy(b, pred);
    //fa_log_info(fa_string("  destroying pair 3"));
    delete_pair(pair);
}

fa_ptr_t fa_pair_first(fa_pair_t pair)
{
    return pair->values[0];
}

fa_ptr_t fa_pair_second(fa_pair_t pair)
{
    return pair->values[1];
}

fa_pair_t fa_pair_duplicate(fa_ptr_t value)
{
    return new_pair(fa_copy(value), fa_copy(value));
}

fa_pair_t fa_pair_swap(fa_pair_t pair)
{
    return new_pair(fa_copy(pair->values[1]), fa_copy(pair->values[0]));
}

fa_pair_t fa_pair_assoc(fa_pair_t p)
{
    fa_ptr_t a = fa_copy(p->values[0]);
    fa_ptr_t b = fa_copy(((fa_pair_t) p->values[1])->values[0]);
    fa_ptr_t c = fa_copy(((fa_pair_t) p->values[1])->values[1]);

    return new_pair(new_pair(a, b), c);
}

fa_pair_t fa_pair_unassoc(fa_pair_t p)
{
    fa_ptr_t a = fa_copy(((fa_pair_t) p->values[0])->values[0]);
    fa_ptr_t b = fa_copy(((fa_pair_t) p->values[0])->values[1]);
    fa_ptr_t c = fa_copy(p->values[1]);
    return new_pair(a, new_pair(b, c));
}


// --------------------------------------------------------------------------------

fa_ptr_t fa_pair_dfirst(fa_pair_t pair)
{
    fa_ptr_t value = pair->values[0];
    fa_destroy(pair);
    return value;
}

fa_ptr_t fa_pair_dsecond(fa_pair_t pair)
{
    fa_ptr_t value = pair->values[1];
    fa_destroy(pair);
    return value;
}

fa_pair_t fa_pair_dduplicate(fa_ptr_t value)
{
    return new_pair(value, fa_copy(value));
}

fa_pair_t fa_pair_dswap(fa_pair_t pair)
{
    fa_pair_t pair2 = new_pair(pair->values[1], pair->values[0]);
    fa_destroy(pair);
    return pair2;
}

fa_pair_t fa_pair_dassoc(fa_pair_t pair)
{
    fa_pair_t pair2 = fa_pair_assoc(pair);
    fa_destroy(pair->values[1]);
    fa_destroy(pair);
    return pair2;
}

fa_pair_t fa_pair_dunassoc(fa_pair_t pair)
{
    fa_pair_t pair2 = fa_pair_unassoc(pair);
    fa_destroy(pair->values[0]);
    fa_destroy(pair);
    return pair2;
}

fa_list_t fa_pair_to_list(fa_pair_t pair)
{
    return list(pair->values[0], pair->values[1]);
}

// --------------------------------------------------------------------------------

bool pair_equal(fa_ptr_t a, fa_ptr_t b)
{
    fa_pair_t c = (fa_pair_t) a;
    fa_pair_t d = (fa_pair_t) b;
    return fa_equal(c->values[0], d->values[0]) && fa_equal(c->values[1], d->values[1]);
}

bool pair_less_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_pair_t c = (fa_pair_t) a;
    fa_pair_t d = (fa_pair_t) b;
    return fa_less_than(c->values[0], d->values[0]) || (fa_equal(c->values[0], d->values[0]) && fa_less_than(c->values[1], d->values[1]));
}

bool pair_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    fa_pair_t c = (fa_pair_t) a;
    fa_pair_t d = (fa_pair_t) b;
    return fa_greater_than(c->values[0], d->values[0]) || (fa_equal(c->values[0], d->values[0]) && fa_greater_than(c->values[1], d->values[1]));
}

fa_string_t pair_show(fa_ptr_t a)
{
    fa_pair_t b = (fa_pair_t) a;
    fa_string_t s = fa_string("(");
    s = fa_string_dappend(s, fa_string_show(b->values[0]));
    s = fa_string_dappend(s, fa_string(","));
    s = fa_string_dappend(s, fa_string_show(b->values[1]));
    s = fa_string_dappend(s, fa_string(")"));
    return s;
}

fa_ptr_t pair_copy(fa_ptr_t a)
{
    return fa_pair_copy(a);
}

fa_ptr_t pair_deep_copy(fa_ptr_t a)
{
    return fa_pair_deep_copy(a);
}

void pair_destroy(fa_ptr_t a)
{
    fa_pair_destroy(a);
}

void pair_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    fa_pair_deep_destroy(a, p);
}

fa_dynamic_type_repr_t pair_get_type(fa_ptr_t a)
{
    return pair_type_repr;
}

fa_ptr_t pair_impl(fa_id_t interface)
{
    static fa_equal_t pair_equal_impl = { pair_equal };
    static fa_order_t pair_order_impl = { pair_less_than, pair_greater_than };
    static fa_string_show_t pair_show_impl = { pair_show };
    static fa_copy_t pair_copy_impl = { pair_copy, pair_deep_copy };
    static fa_destroy_t pair_destroy_impl = { pair_destroy, pair_deep_destroy };
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

void fa_log_pair_count()
{
  fa_log_info(fa_string_dappend(fa_string("Pairs allocated: "), fa_string_dshow(fa_i32(gPairCount))));
}
