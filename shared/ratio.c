
/*
    faudio
    
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/ratio.h>
#include <fa/dynamic.h>
#include <fa/util.h>

typedef fa_ratio_num_t num_t;
typedef fa_ratio_denom_t denom_t;

struct _fa_ratio_t {
    impl_t          impl;       //  Interface dispatcher

    num_t           num;        //  Components
    denom_t         denom;
};


// --------------------------------------------------------------------------------

fa_ratio_t new_ratio()
{
    ptr_t ratio_impl(fa_id_t interface);

    ratio_t p = fa_new(ratio);
    p->impl = &ratio_impl;
    return p;
}

void delete_ratio(fa_ratio_t p)
{
    fa_delete(p);
}

// --------------------------------------------------------------------------------

void normalize_mutable(fa_ratio_t x);

/** Create a rational number.
 */
fa_ratio_t fa_ratio_create(num_t num, denom_t denom)
{
    assert(denom != 0 && "Divide by zero.");

    ratio_t p = new_ratio();
    p->num   = num;
    p->denom = denom;
    normalize_mutable(p);
    return p;
}

/** Copy a rational number.
 */
fa_ratio_t fa_ratio_copy(fa_ratio_t p)
{
    ratio_t q = new_ratio();
    q->num   = p->num;
    q->denom = p->denom;
    return q;
}

/** Destroy a rational number.
 */
void fa_ratio_destroy(fa_ratio_t p)
{
    delete_ratio(p);
}

/** Return the numerator of the given rational number.
 */
num_t fa_ratio_num(fa_ratio_t x)
{
    return x->num;
}

/** Return the denominator of the given rational number.
 */
denom_t fa_ratio_denom(fa_ratio_t x)
{
    return x->denom;
}

/** Destruct the given rational number, writing its numerator
    and denominator to the given locations.
 */
void fa_ratio_match(fa_ratio_t x, num_t *a, denom_t *b)
{
    *a = x->num;
    *b = x->denom;
}


// --------------------------------------------------------------------------------

/** Add the given rational numbers.
 */
fa_ratio_t fa_ratio_add(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * d + b * c, b * d);
}

/** Subtract the given rational numbers.
 */
fa_ratio_t fa_ratio_subtract(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * d - b * c, b * d);
}

/** Multiply the given rational numbers.
 */
fa_ratio_t fa_ratio_multiply(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * c, b * d);
}

/** Divide the given rational numbers.
 */
fa_ratio_t fa_ratio_divide(fa_ratio_t x, fa_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * d, b * c);
}

/** Return the successor of the given rational number.
 */
fa_ratio_t fa_ratio_succ(fa_ratio_t x)
{
    return fa_ratio_add(x, ratio(1, 1));
}

/** Return the predecessor of the given rational number.
 */
fa_ratio_t fa_ratio_pred(fa_ratio_t x)
{
    return fa_ratio_subtract(x, ratio(1, 1));
}

/** Negate the given rational number.
 */
fa_ratio_t fa_ratio_negate(fa_ratio_t x)
{
    return fa_ratio_multiply(x, ratio(-1, 1));
}

/** Invert the given rational number.
 */
fa_ratio_t fa_ratio_recip(fa_ratio_t x)
{
    return fa_ratio_divide(ratio(1, 1), x);
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

/** Normalize the given rational number.
 */
fa_ratio_t fa_ratio_normalize(fa_ratio_t x)
{
    ratio_t y = fa_ratio_copy(x);
    normalize_mutable(y);
    return y;
}

/** Return the absolute value of the given rational number.
 */
fa_ratio_t fa_ratio_absolute(fa_ratio_t x)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    a *= -1;

    if (b < 0) {
        a *= -1;
        b *= -1;
    }

    return ratio(a, b);
}

/** Convert the given rational number to mixed form.

    For example \f$11/3\f$ becomes \f$3+2/3\f$.
 */
void fa_ratio_to_mixed(fa_ratio_t x,
                        fa_ratio_num_t *n,
                        fa_ratio_t     *y)
{
    num_t   a = x->num;
    denom_t b = x->denom;

    *n = a / b;
    *y = ratio(a % b, b);
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
    return fa_ratio_add(a, b);
}

ptr_t ratio_subtract(ptr_t a, ptr_t b)
{
    return fa_ratio_subtract(a, b);
}

ptr_t ratio_multiply(ptr_t a, ptr_t b)
{
    return fa_ratio_multiply(a, b);
}

ptr_t ratio_divide(ptr_t a, ptr_t b)
{
    return fa_ratio_divide(a, b);
}

ptr_t ratio_absolute(ptr_t a)
{
    return fa_ratio_absolute(a);
}

fa_string_t ratio_show(ptr_t a)
{
    // ratio_t b = fa_ratio_normalize(a);
    ratio_t b = a;
    string_t s = string("");

    s = string_dappend(s, fa_string_show(i32(b->num)));
    s = string_dappend(s, string("/"));
    s = string_dappend(s, fa_string_show(i32(b->denom)));

    fa_destroy(b);
    return s;
}

ptr_t ratio_copy(ptr_t a)
{
    return fa_ratio_copy(a);
}

void ratio_destroy(ptr_t a)
{
    fa_ratio_destroy(a);
}

type_repr_t ratio_get_type(fa_ptr_t a)
{
    return ratio_type_repr;
}

ptr_t ratio_impl(fa_id_t interface)
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
        = { ratio_copy };
    static fa_destroy_t ratio_destroy_impl
        = { ratio_destroy };
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

