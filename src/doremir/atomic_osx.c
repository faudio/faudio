
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/atomic.h>
#include <doremir/string.h>
#include <doremir/util.h>

#include <libkern/OSAtomic.h>

/* 
    Strictly 32-bit for now
 */
struct _doremir_atomic_t {
        doremir_impl_t  impl;       /* Interface dispatcher */
        doremir_ptr_t   value;      /* Memory block */
};

doremir_ptr_t atomic_impl(doremir_id_t interface);

doremir_atomic_t doremir_atomic_create()
{
    doremir_atomic_t a = doremir_new(atomic);
    a->impl  = &atomic_impl;
    a->value = 0;
    return a;
}

doremir_atomic_t doremir_atomic_copy(doremir_atomic_t a)
{
    doremir_atomic_t b = doremir_atomic_create();
    b->value = a->value;
    return b;
}

void doremir_atomic_swap(doremir_atomic_t a, doremir_atomic_t b)
{              
    doremir_ptr_t c = a->value;
    a->value        = b->value;   
    b->value        = c;
}

void doremir_atomic_destroy(doremir_atomic_t a)
{
    doremir_delete(a);
}

bool doremir_atomic_exchange(doremir_atomic_t a, doremir_ptr_t old, doremir_ptr_t new)
{
    return OSAtomicCompareAndSwapPtrBarrier(old, new, (doremir_ptr_t) &a->value);
}

void doremir_atomic_add(doremir_atomic_t a, doremir_ptr_t v)
{      
    OSAtomicAdd32Barrier((int32_t) v, (doremir_ptr_t) &a->value);
}

doremir_ptr_t doremir_atomic_get(doremir_atomic_t a)
{
    return (doremir_ptr_t) OSAtomicAdd32Barrier(0, (doremir_ptr_t) &a->value);
}

void doremir_atomic_modify(doremir_atomic_t a, doremir_atomic_updater_t f)
{                  
    bool success = false;
    while (!success)
    {                                        
        doremir_ptr_t currentValue = doremir_atomic_get(a);             
        doremir_ptr_t value = f(currentValue);
        success = doremir_atomic_exchange(a, currentValue, value);
    }
}

void doremir_atomic_set(doremir_atomic_t a, doremir_ptr_t value)
{
    bool success = false;
    while (!success)
    {                                        
        doremir_ptr_t currentValue = doremir_atomic_get(a);
        success = doremir_atomic_exchange(a, currentValue, value);
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
    string_t s = string("<Atomic ");
    s = sdappend(s, doremir_string_format_integer("%02x", (long) a->value));
    s = sdappend(s, string(">"));
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
    
    switch (interface)
    {
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

