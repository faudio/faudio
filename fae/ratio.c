
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/ratio.h>
#include <fae/dynamic.h>
#include <fae/util.h>

typedef fae_ratio_num_t num_t;
typedef fae_ratio_denom_t denom_t;

struct _fae_ratio_t {
    impl_t          impl;       //  Interface dispatcher

    num_t           num;        //  Components
    denom_t         denom;
};


// --------------------------------------------------------------------------------

fae_ratio_t new_ratio()
{
    ptr_t ratio_impl(fae_id_t interface);

    ratio_t p = fae_new(ratio);
    p->impl = &ratio_impl;
    return p;
}

void delete_ratio(fae_ratio_t p)
{
    fae_delete(p);
}

// --------------------------------------------------------------------------------

void normalize_mutable(fae_ratio_t x);

/** Create a rational number.
 */
fae_ratio_t fae_ratio_create(num_t num, denom_t denom)
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
fae_ratio_t fae_ratio_copy(fae_ratio_t p)
{
    ratio_t q = new_ratio();
    q->num   = p->num;
    q->denom = p->denom;
    return q;
}

/** Destroy a rational number.
 */
void fae_ratio_destroy(fae_ratio_t p)
{
    delete_ratio(p);
}

/** Return the numerator of the given rational number.
 */
num_t fae_ratio_num(fae_ratio_t x)
{
    return x->num;
}

/** Return the denominator of the given rational number.
 */
denom_t fae_ratio_denom(fae_ratio_t x)
{
    return x->denom;
}

/** Destruct the given rational number, writing its numerator
    and denominator to the given locations.
 */
void fae_ratio_match(fae_ratio_t x, num_t *a, denom_t *b)
{
    *a = x->num;
    *b = x->denom;
}


// --------------------------------------------------------------------------------

/** Add the given rational numbers.
 */
fae_ratio_t fae_ratio_add(fae_ratio_t x, fae_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * d + b * c, b * d);
}

/** Subtract the given rational numbers.
 */
fae_ratio_t fae_ratio_subtract(fae_ratio_t x, fae_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * d - b * c, b * d);
}

/** Multiply the given rational numbers.
 */
fae_ratio_t fae_ratio_multiply(fae_ratio_t x, fae_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * c, b * d);
}

/** Divide the given rational numbers.
 */
fae_ratio_t fae_ratio_divide(fae_ratio_t x, fae_ratio_t y)
{
    num_t   a = x->num;
    denom_t b = x->denom;
    num_t   c = y->num;
    denom_t d = y->denom;

    return ratio(a * d, b * c);
}

/** Return the successor of the given rational number.
 */
fae_ratio_t fae_ratio_succ(fae_ratio_t x)
{
    return fae_ratio_add(x, ratio(1, 1));
}

/** Return the predecessor of the given rational number.
 */
fae_ratio_t fae_ratio_pred(fae_ratio_t x)
{
    return fae_ratio_subtract(x, ratio(1, 1));
}

/** Negate the given rational number.
 */
fae_ratio_t fae_ratio_negate(fae_ratio_t x)
{
    return fae_ratio_multiply(x, ratio(-1, 1));
}

/** Invert the given rational number.
 */
fae_ratio_t fae_ratio_recip(fae_ratio_t x)
{
    return fae_ratio_divide(ratio(1, 1), x);
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

void normalize_mutable(fae_ratio_t x)
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
fae_ratio_t fae_ratio_normalize(fae_ratio_t x)
{
    ratio_t y = fae_ratio_copy(x);
    normalize_mutable(y);
    return y;
}

/** Return the absolute value of the given rational number.
 */
fae_ratio_t fae_ratio_absolute(fae_ratio_t x)
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
void fae_ratio_to_mixed(fae_ratio_t x,
                        fae_ratio_num_t *n,
                        fae_ratio_t     *y)
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
    return fae_ratio_add(a, b);
}

ptr_t ratio_subtract(ptr_t a, ptr_t b)
{
    return fae_ratio_subtract(a, b);
}

ptr_t ratio_multiply(ptr_t a, ptr_t b)
{
    return fae_ratio_multiply(a, b);
}

ptr_t ratio_divide(ptr_t a, ptr_t b)
{
    return fae_ratio_divide(a, b);
}

ptr_t ratio_absolute(ptr_t a)
{
    return fae_ratio_absolute(a);
}

fae_string_t ratio_show(ptr_t a)
{
    // ratio_t b = fae_ratio_normalize(a);
    ratio_t b = a;
    string_t s = string("");

    s = string_dappend(s, fae_string_show(i32(b->num)));
    s = string_dappend(s, string("/"));
    s = string_dappend(s, fae_string_show(i32(b->denom)));

    fae_destroy(b);
    return s;
}

ptr_t ratio_copy(ptr_t a)
{
    return fae_ratio_copy(a);
}

void ratio_destroy(ptr_t a)
{
    fae_ratio_destroy(a);
}

type_repr_t ratio_get_type(fae_ptr_t a)
{
    return ratio_type_repr;
}

ptr_t ratio_impl(fae_id_t interface)
{
    static fae_equal_t ratio_equal_impl
        = { ratio_equal };
    static fae_order_t ratio_order_impl
        = { ratio_less_than, ratio_greater_than };
    static fae_string_show_t ratio_show_impl
        = { ratio_show };
    static fae_number_t  ratio_number_impl
        = { ratio_add, ratio_subtract, ratio_multiply, ratio_divide, ratio_absolute };
    static fae_copy_t ratio_copy_impl
        = { ratio_copy };
    static fae_destroy_t ratio_destroy_impl
        = { ratio_destroy };
    static fae_dynamic_t ratio_dynamic_impl = { ratio_get_type };

    switch (interface) {
    case fae_equal_i:
        return &ratio_equal_impl;

    case fae_order_i:
        return &ratio_order_impl;

    case fae_string_show_i:
        return &ratio_show_impl;

    case fae_number_i:
        return &ratio_number_impl;

    case fae_copy_i:
        return &ratio_copy_impl;

    case fae_destroy_i:
        return &ratio_destroy_impl;

    case fae_dynamic_i:
        return &ratio_dynamic_impl;

    default:
        return NULL;
    }
}

