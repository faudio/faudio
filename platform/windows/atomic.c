
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/atomic.h>
#include <fa/string.h>
#include <fa/util.h>

#include <Windows.h>

/*
    TODO these are strictly 32-bit for now
 */
struct _fa_atomic_t {
    fa_impl_t      impl;       //  Interface dispatcher
    intptr_t    value;
};

fa_ptr_t atomic_impl(fa_id_t);

fa_atomic_t fa_atomic_create()
{
    fa_atomic_t a = fa_new(atomic);
    a->impl     = &atomic_impl;
    a->value    = (intptr_t)0;

    return a;
}

fa_atomic_t fa_atomic_copy(fa_atomic_t a)
{
    fa_atomic_t b  = fa_atomic_create();
    b->value    = a->value;
    return b;
}

/*
void fa_atomic_swap(fa_atomic_t a, fa_atomic_t b){}
*/

void fa_atomic_destroy(fa_atomic_t a)
{
    fa_delete(a);
}

bool fa_atomic_exchange(fa_atomic_t a, fa_ptr_t pold, fa_ptr_t pnew)
{
    return (pold == InterlockedCompareExchangePointer((fa_ptr_t)&a->value, pnew, pold));
}

void fa_atomic_add(fa_atomic_t a, int32_t v)
{
#ifdef __MINGW32__
    InterlockedExchangeAdd((LONG *)&a->value, (LONG)v);
#else
#error "only 32 bit supported, needs MINGW64"
    // InterlockedExchangeAdd64((LONGLONG*)&a->value, (LONGLONG)v);
#endif
}

void *fa_atomic_get(fa_atomic_t a)
{
#ifdef __MINGW32__
    return InterlockedCompareExchange((LONG *)&a->value, 0L, 0L);
#else
#error "only 32 bit supported needs MINGW64"
    // InterlockedCompareExchange64((LONGLONG*)&a->value, 0LL, 0LL);
#endif
}

void fa_atomic_modify(fa_atomic_t atomic, fa_unary_t func, fa_ptr_t data)
{
    bool result = false;

    while (!result) {
        fa_ptr_t state = fa_atomic_get(atomic);
        fa_ptr_t value = func(state, data);
        result = fa_atomic_exchange(atomic, state, value);
    }
}

void fa_atomic_set(fa_atomic_t atomic, fa_ptr_t value)
{
    bool result = false;

    while (!result) {
        fa_ptr_t state = fa_atomic_get(atomic);
        result = fa_atomic_exchange(atomic, state, value);
    }
}

// --------------------------------------------------------------------------------

bool atomic_equal(fa_ptr_t a, fa_ptr_t b)
{
    return fa_equal(fa_atomic_get(a), fa_atomic_get(b));
}

bool atomic_less_than(fa_ptr_t a, fa_ptr_t b)
{
    return fa_less_than(fa_atomic_get(a), fa_atomic_get(b));
}

bool atomic_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    return fa_greater_than(fa_atomic_get(a), fa_atomic_get(b));
}

fa_string_t atomic_show(fa_ptr_t v)
{
    fa_atomic_t a = (fa_atomic_t) v;
    fa_string_t s = string("<Atomic");
    s = string_dappend(s, fa_string_format_integral(" %02x", (LONG) a->value));
    s = string_dappend(s, string(">"));
    return s;
}

fa_ptr_t atomic_copy(fa_ptr_t a)
{
    return fa_atomic_copy(a);
}

void atomic_destroy(fa_ptr_t a)
{
    fa_atomic_destroy(a);
}

fa_ptr_t atomic_impl(fa_id_t iface)
{
    static fa_equal_t atomic_equal_impl = { atomic_equal };
    static fa_order_t atomic_order_impl = { atomic_less_than, atomic_greater_than };
    static fa_string_show_t atomic_show_impl = { atomic_show };
    static fa_copy_t atomic_copy_impl = { atomic_copy };
    static fa_destroy_t atomic_destroy_impl = { atomic_destroy };

    switch (iface) {
    case fa_equal_i:
        return &atomic_equal_impl;

    case fa_order_i:
        return &atomic_order_impl;

    case fa_string_show_i:
        return &atomic_show_impl;

    case fa_copy_i:
        return &atomic_copy_impl;

    case fa_destroy_i:
        return &atomic_destroy_impl;

    default:
        return NULL;
    }
}

