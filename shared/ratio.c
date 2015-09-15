
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/ratio.h>
#include <fa/dynamic.h>
#include <fa/util.h>

typedef fa_ratio_num_t    num_t;
typedef fa_ratio_denom_t  denom_t;

struct _fa_ratio_t {
    fa_impl_t       impl;       //  Interface dispatcher

    num_t           num;        //  Components
    denom_t         denom;
};


// --------------------------------------------------------------------------------

fa_ratio_t new_ratio()
{
    fa_ptr_t ratio_impl(fa_id_t interface);

    fa_ratio_t p = fa_new(ratio);
    p->impl = &ratio_impl;
    return p;
}

void delete_ratio(fa_ratio_t p)
{
    fa_delete(p);
}

// --------------------------------------------------------------------------------

void normalize_mutable(fa_ratio_t x);

fa_ratio_t fa_ratio_create(num_t num, denom_t denom)
{
    assert(denom != 0 && "Divide by zero.");

    fa_ratio_t p = new_ratio();
    p->num   = num;
    p->denom = denom;
    normalize_mutable(p);
    return p;
}

fa_ratio_t fa_ratio_copy(fa_ratio_t p)
{
    fa_ratio_t q = new_ratio();
    q->num   = p->num;
    q->denom = p->denom;
    return q;
}

void fa_ratio_destroy(fa_ratio_t p)
{
    delete_ratio(p);
}

num_t fa_ratio_num(fa_ratio_t x)
{
    return x->num;
}

denom_t fa_ratio_denom(fa_ratio_t x)
{
    return x->denom;
}

void fa_ratio_match(fa_ratio_t x, num_t *a, denom_t *b)
{
    *a = x->num;
    *b = x->denom;
}


// --------------------------------------------------------------------------------

double to_double(fa_ratio_t x)
{
    return ((double) x->num) / ((double) x->denom);
}

fa_ratio_t fa_ratio_add(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return fa_ratio(a * d + b * c, b * d);
}

fa_ratio_t fa_ratio_add_dither(fa_ratio_t x, fa_ratio_t y)
{
    double x2 = to_double(x);
    double y2 = to_double(y);
    double res = x2 + y2;
    return fa_ratio(res * 10000, 10000);
}

fa_ratio_t fa_ratio_add_safe(fa_ratio_t x, fa_ratio_t y)
{
    if (x->num > 1000 || x->denom > 1000 || y->num > 1000 || y->denom > 1000) {
        return fa_ratio_add_dither(x, y);
    } else {
        return fa_ratio_add(x, y);
    }
}

fa_ratio_t fa_ratio_subtract(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return fa_ratio(a * d - b * c, b * d);
}

fa_ratio_t fa_ratio_multiply(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return fa_ratio(a * c, b * d);
}

fa_ratio_t fa_ratio_divide(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return fa_ratio(a * d, b * c);
}

fa_ratio_t fa_ratio_succ(fa_ratio_t x)
{
    return fa_ratio_add(x, fa_ratio(1, 1));
}

fa_ratio_t fa_ratio_pred(fa_ratio_t x)
{
    return fa_ratio_subtract(x, fa_ratio(1, 1));
}

fa_ratio_t fa_ratio_negate(fa_ratio_t x)
{
    return fa_ratio_multiply(x, fa_ratio(-1, 1));
}

fa_ratio_t fa_ratio_recip(fa_ratio_t x)
{
    return fa_ratio_divide(fa_ratio(1, 1), x);
}

inline static int gcd(int x, int y)
{
    x = abs(x);
    y = abs(y);

    while (y) {
        int t = y;
        y = x % y;
        x = t;
    }

    return x;
}

void normalize_mutable(fa_ratio_t x)
{
    if (x->denom < 0) {
        x->num   = -x->num;
        x->denom = -x->denom;
    }

    int n = gcd(x->num, x->denom);

    if (n > 1) {
        x->num   /= n;
        x->denom /= n;
    }
}

fa_ratio_t fa_ratio_normalize(fa_ratio_t x)
{
    fa_ratio_t y = fa_ratio_copy(x);
    normalize_mutable(y);
    return y;
}

fa_ratio_t fa_ratio_absolute(fa_ratio_t x)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    a *= -1;

    if (b < 0) {
        a *= -1;
        b *= -1;
    }

    return fa_ratio(a, b);
}

void fa_ratio_to_mixed(fa_ratio_t x,
                       fa_ratio_num_t *n,
                       fa_ratio_t     *y)
{
    num_t   a = x->num;
    denom_t b = x->denom;

    *n = a / b;
    *y = fa_ratio(a % b, b);
}



// --------------------------------------------------------------------------------

bool ratio_equal(fa_ptr_t m, fa_ptr_t n)
{
    fa_ratio_t x = (fa_ratio_t) m;
    fa_ratio_t y = (fa_ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d == b * c;
}

bool ratio_less_than(fa_ptr_t m, fa_ptr_t n)
{
    fa_ratio_t x = (fa_ratio_t) m;
    fa_ratio_t y = (fa_ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d < b * c;
}

bool ratio_greater_than(fa_ptr_t m, fa_ptr_t n)
{
    fa_ratio_t x = (fa_ratio_t) m;
    fa_ratio_t y = (fa_ratio_t) n;

    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return a * d > b * c;
}

fa_ptr_t ratio_add(fa_ptr_t a, fa_ptr_t b)
{
    return fa_ratio_add(a, b);
}

fa_ptr_t ratio_subtract(fa_ptr_t a, fa_ptr_t b)
{
    return fa_ratio_subtract(a, b);
}

fa_ptr_t ratio_multiply(fa_ptr_t a, fa_ptr_t b)
{
    return fa_ratio_multiply(a, b);
}

fa_ptr_t ratio_divide(fa_ptr_t a, fa_ptr_t b)
{
    return fa_ratio_divide(a, b);
}

fa_ptr_t ratio_absolute(fa_ptr_t a)
{
    return fa_ratio_absolute(a);
}

fa_string_t ratio_show(fa_ptr_t a)
{
    fa_ratio_t b = a; // just a typecast
	if (b->num == 0) {
		return fa_string("0");
	}
	fa_string_t s = fa_string("<");
    s = fa_string_dappend(s, fa_string_dshow(fa_i32(b->num)));
	s = fa_string_dappend(s, fa_string("/"));
    s = fa_string_dappend(s, fa_string_dshow(fa_i32(b->denom)));
	s = fa_string_dappend(s, fa_string(">"));
    return s;
}

fa_ptr_t ratio_copy(fa_ptr_t a)
{
    return fa_ratio_copy(a);
}

fa_ptr_t ratio_deep_copy(fa_ptr_t a)
{
    return fa_ratio_copy(a);
}

void ratio_destroy(fa_ptr_t a)
{
    fa_ratio_destroy(a);
}

void ratio_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    if (p(a)) fa_ratio_destroy(a);
}

fa_dynamic_type_repr_t ratio_get_type(fa_ptr_t a)
{
    return ratio_type_repr;
}

fa_ptr_t ratio_impl(fa_id_t interface)
{
    static fa_equal_t ratio_equal_impl
        = { ratio_equal };
    static fa_order_t ratio_order_impl
        = { ratio_less_than, ratio_greater_than };
    static fa_string_show_t ratio_show_impl
        = { ratio_show };
    static fa_number_t  ratio_number_impl
        = { ratio_add, ratio_subtract, ratio_multiply, ratio_divide, ratio_absolute };
    static fa_copy_t ratio_copy_impl
        = { ratio_copy, ratio_deep_copy };
    static fa_destroy_t ratio_destroy_impl
        = { ratio_destroy, ratio_deep_destroy };
    static fa_dynamic_t ratio_dynamic_impl = { ratio_get_type };

    switch (interface) {
    case fa_equal_i:
        return &ratio_equal_impl;

    case fa_order_i:
        return &ratio_order_impl;

    case fa_string_show_i:
        return &ratio_show_impl;

    case fa_number_i:
        return &ratio_number_impl;

    case fa_copy_i:
        return &ratio_copy_impl;

    case fa_destroy_i:
        return &ratio_destroy_impl;

    case fa_dynamic_i:
        return &ratio_dynamic_impl;

    default:
        return NULL;
    }
}

