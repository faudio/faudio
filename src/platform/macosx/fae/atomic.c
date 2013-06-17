
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/atomic.h>
#include <doremir/string.h>
#include <doremir/util.h>

#include <libkern/OSAtomic.h>

struct _doremir_atomic_t {
    impl_t      impl;       //  Interface dispatcher
    ptr_t       value;      //  Memory block
};


doremir_ptr_t atomic_impl(doremir_id_t interface);

/** Create a new atomic reference.
    @par Atomicity
        Non-atomic
 */
doremir_atomic_t doremir_atomic_create()
{
    atomic_t a = doremir_new(atomic);

    a->impl  = &atomic_impl;
    a->value = NULL;

    return a;
}

/** Copy the given atomic reference.
    @par Atomicity
        Non-atomic
 */
doremir_atomic_t doremir_atomic_copy(doremir_atomic_t a)
{
    atomic_t b = doremir_atomic_create();
    b->value = a->value;
    return b;
}

/** Swap contents of the given atomic reference.
    @par Atomicity
        Non-atomic
 */
void doremir_atomic_swap(doremir_atomic_t a, doremir_atomic_t b)
{
    ptr_t x  = a->value;
    a->value = b->value;
    b->value = x;
}

/** Destroy the given atomic reference.
    @par Atomicity
        Non-atomic
 */
void doremir_atomic_destroy(doremir_atomic_t a)
{
    doremir_delete(a);
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
bool doremir_atomic_exchange(doremir_atomic_t a, doremir_ptr_t old, doremir_ptr_t new)
{
    return OSAtomicCompareAndSwapPtrBarrier(old, new, (ptr_t) &a->value);
}

/** Return the current value of the given atomic reference.
    @par Atomicity
        Atomic
 */
doremir_ptr_t doremir_atomic_get(doremir_atomic_t a)
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
void doremir_atomic_modify(doremir_atomic_t atomic, doremir_unary_t func, doremir_ptr_t data)
{
    bool result = false;

    while (!result) {
        ptr_t state = doremir_atomic_get(atomic);
        ptr_t value = func(state, data);
        result = doremir_atomic_exchange(atomic, state, value);
    }
}

/** Set the given given atomic reference.
    
    @par Atomicity Atomic
 */
void doremir_atomic_set(doremir_atomic_t atomic, doremir_ptr_t value)
{
    bool result = false;

    while (!result) {
        ptr_t state = doremir_atomic_get(atomic);
        result = doremir_atomic_exchange(atomic, state, value);
    }
}


// --------------------------------------------------------------------------------

bool atomic_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_equal(doremir_atomic_get(a), doremir_atomic_get(b));
}

bool atomic_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_less_than(doremir_atomic_get(a), doremir_atomic_get(b));
}

bool atomic_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_greater_than(doremir_atomic_get(a), doremir_atomic_get(b));
}

doremir_string_t atomic_show(doremir_ptr_t v)
{
    doremir_atomic_t a = (doremir_atomic_t) v;
    string_t s = string("<Atomic");
    s = string_dappend(s, doremir_string_format_integral(" %02x", (long) a->value));
    s = string_dappend(s, string(">"));
    return s;
}

doremir_ptr_t atomic_copy(doremir_ptr_t a)
{
    return doremir_atomic_copy(a);
}

void atomic_destroy(doremir_ptr_t a)
{
    doremir_atomic_destroy(a);
}


doremir_ptr_t atomic_impl(doremir_id_t interface)
{
    static doremir_equal_t atomic_equal_impl = { atomic_equal };
    static doremir_order_t atomic_order_impl = { atomic_less_than, atomic_greater_than };
    static doremir_string_show_t atomic_show_impl = { atomic_show };
    static doremir_copy_t atomic_copy_impl = { atomic_copy };
    static doremir_destroy_t atomic_destroy_impl = { atomic_destroy };

    switch (interface) {
    case doremir_equal_i:
        return &atomic_equal_impl;

    case doremir_order_i:
        return &atomic_order_impl;

    case doremir_string_show_i:
        return &atomic_show_impl;

    case doremir_copy_i:
        return &atomic_copy_impl;

    case doremir_destroy_i:
        return &atomic_destroy_impl;

    default:
        return NULL;
    }
}

