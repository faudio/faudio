
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/atomic.h>
#include <fae/string.h>
#include <fae/util.h>

#include <libkern/OSAtomic.h>

struct _fae_atomic_t {
    impl_t      impl;       //  Interface dispatcher
    ptr_t       value;      //  Memory block
};


fae_ptr_t atomic_impl(fae_id_t interface);

/** Create a new atomic reference.
    @par Atomicity
        Non-atomic
 */
fae_atomic_t fae_atomic_create()
{
    atomic_t a = fae_new(atomic);

    a->impl  = &atomic_impl;
    a->value = NULL;

    return a;
}

/** Copy the given atomic reference.
    @par Atomicity
        Non-atomic
 */
fae_atomic_t fae_atomic_copy(fae_atomic_t a)
{
    atomic_t b = fae_atomic_create();
    b->value = a->value;
    return b;
}

/** Swap contents of the given atomic reference.
    @par Atomicity
        Non-atomic
 */
void fae_atomic_swap(fae_atomic_t a, fae_atomic_t b)
{
    ptr_t x  = a->value;
    a->value = b->value;
    b->value = x;
}

/** Destroy the given atomic reference.
    @par Atomicity
        Non-atomic
 */
void fae_atomic_destroy(fae_atomic_t a)
{
    fae_delete(a);
}


// --------------------------------------------------------------------------------

/** Compares the given value with the current value of the given atomic reference,
    replacing it if successful.

    @param a   The atomic reference.
    @param old Old value.
    @param new New value.
    @return
        Whether comparison was successful, i.e. whether the values where exchanged.
    @par Atomicity
        Atomic
 */
bool fae_atomic_exchange(fae_atomic_t a, fae_ptr_t old, fae_ptr_t new)
{
    return OSAtomicCompareAndSwapPtrBarrier(old, new, (ptr_t) &a->value);
}

/** Return the current value of the given atomic reference.
    @par Atomicity
        Atomic
 */
fae_ptr_t fae_atomic_get(fae_atomic_t a)
{
#if (DOREMIR_ARCH_BITS == 32)
    return (ptr_t) OSAtomicAdd32Barrier(0, (ptr_t) &a->value);
#else
    return (ptr_t) OSAtomicAdd64Barrier(0, (ptr_t) &a->value);
#endif
}

/** Update the given atomic value by applying the given pure function.

    @param atomic   Atomic reference.
    @param func     Function to be applied to the value.
    @param data     Value to be passed to the function.

    @par Atomicity Atomic
 */
void fae_atomic_modify(fae_atomic_t atomic, fae_unary_t func, fae_ptr_t data)
{
    bool result = false;

    while (!result) {
        ptr_t state = fae_atomic_get(atomic);
        ptr_t value = func(state, data);
        result = fae_atomic_exchange(atomic, state, value);
    }
}

/** Set the given given atomic reference.

    @par Atomicity Atomic
 */
void fae_atomic_set(fae_atomic_t atomic, fae_ptr_t value)
{
    bool result = false;

    while (!result) {
        ptr_t state = fae_atomic_get(atomic);
        result = fae_atomic_exchange(atomic, state, value);
    }
}


// --------------------------------------------------------------------------------

bool atomic_equal(fae_ptr_t a, fae_ptr_t b)
{
    return fae_equal(fae_atomic_get(a), fae_atomic_get(b));
}

bool atomic_less_than(fae_ptr_t a, fae_ptr_t b)
{
    return fae_less_than(fae_atomic_get(a), fae_atomic_get(b));
}

bool atomic_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    return fae_greater_than(fae_atomic_get(a), fae_atomic_get(b));
}

fae_string_t atomic_show(fae_ptr_t v)
{
    fae_atomic_t a = (fae_atomic_t) v;
    string_t s = string("<Atomic");
    s = string_dappend(s, fae_string_format_integral(" %02x", (long) a->value));
    s = string_dappend(s, string(">"));
    return s;
}

fae_ptr_t atomic_copy(fae_ptr_t a)
{
    return fae_atomic_copy(a);
}

void atomic_destroy(fae_ptr_t a)
{
    fae_atomic_destroy(a);
}


fae_ptr_t atomic_impl(fae_id_t interface)
{
    static fae_equal_t atomic_equal_impl = { atomic_equal };
    static fae_order_t atomic_order_impl = { atomic_less_than, atomic_greater_than };
    static fae_string_show_t atomic_show_impl = { atomic_show };
    static fae_copy_t atomic_copy_impl = { atomic_copy };
    static fae_destroy_t atomic_destroy_impl = { atomic_destroy };

    switch (interface) {
    case fae_equal_i:
        return &atomic_equal_impl;

    case fae_order_i:
        return &atomic_order_impl;

    case fae_string_show_i:
        return &atomic_show_impl;

    case fae_copy_i:
        return &atomic_copy_impl;

    case fae_destroy_i:
        return &atomic_destroy_impl;

    default:
        return NULL;
    }
}

