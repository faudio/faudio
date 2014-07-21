
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/atomic.h>
#include <fa/string.h>
#include <fa/util.h>

#include <libkern/OSAtomic.h>

struct _fa_atomic_t {
    impl_t      impl;       //  Interface dispatcher
    ptr_t       value;      //  Memory block
};


fa_ptr_t atomic_impl(fa_id_t interface);

fa_atomic_t fa_atomic_create()
{
    atomic_t a = fa_new(atomic);

    a->impl  = &atomic_impl;
    a->value = NULL;

    return a;
}

fa_atomic_t fa_atomic_copy(fa_atomic_t a)
{
    atomic_t b = fa_atomic_create();
    b->value = a->value;
    return b;
}

void fa_atomic_destroy(fa_atomic_t a)
{
    fa_delete(a);
}


// --------------------------------------------------------------------------------

bool fa_atomic_exchange(fa_atomic_t a, fa_ptr_t old, fa_ptr_t new)
{
    return OSAtomicCompareAndSwapPtrBarrier(old, new, (ptr_t) &a->value);
}

fa_ptr_t fa_atomic_get(fa_atomic_t a)
{
#ifdef __i386__
    return (ptr_t) OSAtomicAdd32Barrier(0, (ptr_t) &a->value);
#else
#ifdef __x86_64__
    return (ptr_t) OSAtomicAdd64Barrier(0, (ptr_t) &a->value);
#else
#error "Unknown architecture"
#endif
#endif
}

void fa_atomic_add(fa_atomic_t a, int32_t x)
{
#ifdef __i386__
    OSAtomicAdd32Barrier(x, (ptr_t) &a->value);
#else
#ifdef __x86_64__
    OSAtomicAdd64Barrier(x, (ptr_t) &a->value);
#else
#error "Unknown architecture"
#endif
#endif
}


void fa_atomic_modify(fa_atomic_t atomic, fa_unary_t func, fa_ptr_t data)
{
    bool result = false;

    while (!result) {
        ptr_t state = fa_atomic_get(atomic);
        ptr_t value = func(state, data);
        result = fa_atomic_exchange(atomic, state, value);
    }
}

void fa_atomic_set(fa_atomic_t atomic, fa_ptr_t value)
{
    bool result = false;

    while (!result) {
        ptr_t state = fa_atomic_get(atomic);
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
    string_t s = fa_string("<Atomic");
    s = fa_string_dappend(s, fa_string_format_integral(" %02x", (long) a->value));
    s = fa_string_dappend(s, fa_string(">"));
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


fa_ptr_t atomic_impl(fa_id_t interface)
{
    static fa_equal_t atomic_equal_impl = { atomic_equal };
    static fa_order_t atomic_order_impl = { atomic_less_than, atomic_greater_than };
    static fa_string_show_t atomic_show_impl = { atomic_show };
    static fa_copy_t atomic_copy_impl = { atomic_copy };
    static fa_destroy_t atomic_destroy_impl = { atomic_destroy };

    switch (interface) {
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

