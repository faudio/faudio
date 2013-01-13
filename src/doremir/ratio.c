
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
        impl_t          impl;       //  Interface dispatcher

        num_t           num;        //  Components
        denom_t         denom;
    };

ptr_t ratio_impl(doremir_id_t interface);

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

// --------------------------------------------------------------------------------

ratio_t doremir_ratio_create(num_t num, denom_t denom)
{
    ratio_t p = new_ratio();
    p->num   = num;
    p->denom = denom;
    return p;
}

ratio_t doremir_ratio_copy(ratio_t p)
{
    ratio_t q = new_ratio();
    q->num   = p->num;
    q->denom = p->denom;
    return q;
}

void doremir_ratio_destroy(ratio_t p)
{
    delete_ratio(p);
}


// --------------------------------------------------------------------------------

num_t doremir_ratio_num(ratio_t x)
{
    return x->num;
}

denom_t doremir_ratio_denom(ratio_t x)
{
    return x->denom;
}

void doremir_ratio_match(ratio_t x, num_t *a, denom_t *b)
{
    *a = x->num;
    *b = x->denom;
}


// --------------------------------------------------------------------------------

ratio_t doremir_ratio_add(ratio_t x, ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*d + b*c, b*d );
}

ratio_t doremir_ratio_subtract(ratio_t x, ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*d - b*c, b*d );
}

ratio_t doremir_ratio_multiply(ratio_t x, ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*c, b*d );
}

ratio_t doremir_ratio_divide(ratio_t x, ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio( a*d, b*c );
}

ratio_t doremir_ratio_succ(ratio_t x)
{
    return doremir_ratio_add(x, ratio(1,1));
}

ratio_t doremir_ratio_pred(ratio_t x)
{
    return doremir_ratio_subtract(x, ratio(1,1));
}

ratio_t doremir_ratio_negate(ratio_t x)
{
    return doremir_ratio_multiply(x, ratio(-1,1));
}

ratio_t doremir_ratio_recip(ratio_t x)
{
    return doremir_ratio_divide(ratio(1,1), x);
}

ratio_t doremir_ratio_normalize(ratio_t x)
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

ratio_t doremir_ratio_absolute(ratio_t x)
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

bool ratio_equal(ptr_t m, ptr_t n)
{
    ratio_t x = (ratio_t) m;
    ratio_t y = (ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d == b * c;
}

bool ratio_less_than(ptr_t m, ptr_t n)
{
    ratio_t x = (ratio_t) m;
    ratio_t y = (ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d < b * c;
}

bool ratio_greater_than(ptr_t m, ptr_t n)
{
    ratio_t x = (ratio_t) m;
    ratio_t y = (ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d > b * c;
}

ptr_t ratio_add(ptr_t a, ptr_t b)
{
    return doremir_ratio_add(a, b);
}

ptr_t ratio_subtract(ptr_t a, ptr_t b)
{
    return doremir_ratio_subtract(a, b);
}

ptr_t ratio_multiply(ptr_t a, ptr_t b)
{
    return doremir_ratio_multiply(a, b);
}

ptr_t ratio_divide(ptr_t a, ptr_t b)
{
    return doremir_ratio_divide(a, b);
}

ptr_t ratio_absolute(ptr_t a)
{
    return doremir_ratio_absolute(a);
}

doremir_string_t ratio_show(ptr_t a)
{
    ratio_t b = (ratio_t) a;
    string_t s = string("");
    
    s = string_dappend(s, doremir_string_show(i32(b->num)));
    s = string_dappend(s, string("/"));
    s = string_dappend(s, doremir_string_show(i32(b->denom)));
    
    return s;
}

ptr_t ratio_copy(ptr_t a)
{
    return doremir_ratio_copy(a);
}

void ratio_destroy(ptr_t a)
{
    doremir_ratio_destroy(a);
}

ptr_t ratio_impl(doremir_id_t interface)
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

