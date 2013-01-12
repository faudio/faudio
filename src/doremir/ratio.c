
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/ratio.h>
#include <doremir/util.h>

typedef doremir_ratio_num_t num_t;
typedef doremir_ratio_denom_t denom_t;

struct _doremir_ratio_t {
        impl_t          impl;       /* Interface dispatcher */
        num_t           num;
        denom_t         denom;
    };

doremir_ptr_t ratio_impl(doremir_id_t interface);

ratio_t new_ratio()
{
    ratio_t p = doremir_new(ratio);
    p->impl = &ratio_impl;
    return p;
}
void delete_ratio(ratio_t p)
{
    doremir_delete(p);
}

doremir_ratio_t doremir_ratio_create(num_t num, denom_t denom)
{
    ratio_t p = new_ratio();
    p->num   = num;
    p->denom = denom;
    return p;
}

doremir_ratio_t doremir_ratio_copy(doremir_ratio_t p)
{
    ratio_t q = new_ratio();
    q->num   = p->num;
    q->denom = p->denom;
    return q;
}

void doremir_ratio_destroy(doremir_ratio_t p)
{
    delete_ratio(p);
}


// --------------------------------------------------------------------------------

doremir_ratio_num_t doremir_ratio_num(doremir_ratio_t x)
{
    return x->num;
}

doremir_ratio_denom_t doremir_ratio_denom(doremir_ratio_t x)
{
    return x->denom;
}

void doremir_ratio_match(doremir_ratio_t x,
                         doremir_ratio_num_t *a,
                         doremir_ratio_denom_t *b)
{
    *a = x->num;
    *b = x->denom;
}


// --------------------------------------------------------------------------------

doremir_ratio_t doremir_ratio_add(doremir_ratio_t x,
                                  doremir_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*d + b*c, b*d );
}

doremir_ratio_t doremir_ratio_subtract(doremir_ratio_t x,
                                       doremir_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*d - b*c, b*d );
}

doremir_ratio_t doremir_ratio_multiply(doremir_ratio_t x,
                                       doremir_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*c, b*d );
}

doremir_ratio_t doremir_ratio_divide(doremir_ratio_t x,
                                     doremir_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*d, b*c );
}

doremir_ratio_t doremir_ratio_succ(doremir_ratio_t x)
{
    return doremir_ratio_add(x, ratio(1,1));
}

doremir_ratio_t doremir_ratio_pred(doremir_ratio_t x)
{
    return doremir_ratio_subtract(x, ratio(1,1));
}

doremir_ratio_t doremir_ratio_negate(doremir_ratio_t x)
{
    return doremir_ratio_multiply(x, ratio(-1,1));
}

doremir_ratio_t doremir_ratio_recip(doremir_ratio_t x)
{
    return doremir_ratio_divide(ratio(1,1), x);
}

doremir_ratio_t doremir_ratio_normalize(doremir_ratio_t x)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    if (b < 0)
    {
        a *= -1;
        b *= -1;
    }
    return ratio( a, b );
}

doremir_ratio_t doremir_ratio_absolute(doremir_ratio_t x)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    a *= -1;
    if (b < 0)
    {
        a *= -1;
        b *= -1;
    }
    return ratio( a, b );
}


// --------------------------------------------------------------------------------

bool ratio_equal(doremir_ptr_t m, doremir_ptr_t n)
{
    ratio_t x = (ratio_t) m;
    ratio_t y = (ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d == b * c;
}

bool ratio_less_than(doremir_ptr_t m, doremir_ptr_t n)
{
    ratio_t x = (ratio_t) m;
    ratio_t y = (ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d < b * c;
}

bool ratio_greater_than(doremir_ptr_t m, doremir_ptr_t n)
{
    ratio_t x = (ratio_t) m;
    ratio_t y = (ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d > b * c;
}

doremir_ptr_t ratio_add(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_ratio_add(a, b);
}

doremir_ptr_t ratio_subtract(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_ratio_subtract(a, b);
}

doremir_ptr_t ratio_multiply(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_ratio_multiply(a, b);
}

doremir_ptr_t ratio_divide(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_ratio_divide(a, b);
}

doremir_ptr_t ratio_absolute(doremir_ptr_t a)
{
    return doremir_ratio_absolute(a);
}

doremir_string_t ratio_show(doremir_ptr_t a)
{
    ratio_t b = (ratio_t) a;
    string_t s = string("");
    s = sdappend(s, sshow(i32(b->num)));
    s = sdappend(s, string("/"));
    s = sdappend(s, sshow(i32(b->denom)));
    return s;
}

doremir_ptr_t ratio_copy(doremir_ptr_t a)
{
    return doremir_ratio_copy(a);
}

void ratio_destroy(doremir_ptr_t a)
{
    doremir_ratio_destroy(a);
}

doremir_ptr_t ratio_impl(doremir_id_t interface)
{
    static doremir_equal_t ratio_equal_impl 
        = { ratio_equal };
    static doremir_order_t ratio_order_impl 
        = { ratio_less_than, ratio_greater_than };
    static doremir_string_show_t ratio_show_impl 
        = { ratio_show };
    static doremir_number_t  ratio_number_impl 
        = { ratio_add, ratio_subtract, ratio_multiply, ratio_divide, ratio_absolute };
    static doremir_copy_t ratio_copy_impl 
        = { ratio_copy };
    static doremir_destroy_t ratio_destroy_impl 
        = { ratio_destroy };

    switch (interface)
    {
    case doremir_equal_i:
        return &ratio_equal_impl;

    case doremir_order_i:
        return &ratio_order_impl;

    case doremir_string_show_i:
        return &ratio_show_impl;

    case doremir_number_i:
        return &ratio_number_impl;

    case doremir_copy_i:
        return &ratio_copy_impl;

    case doremir_destroy_i:
        return &ratio_destroy_impl;

    default:
        return NULL;
    }
}

