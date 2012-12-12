
#include <libkern/OSAtomic.h>
#include <doremir/atomic.h>

/* 
    Strictly 32-bit for now
 */
struct _doremir_atomic_t
{
    doremir_ptr_t value;
};

doremir_atomic_t doremir_atomic_create()
{
    doremir_atomic_t a = malloc(sizeof(struct _doremir_atomic_t));
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
    doremir_ptr_t tmp = b->value;
    a->value = b->value;   
    a->value = tmp;
}

void doremir_atomic_destroy(doremir_atomic_t a)
{
    free(a);
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

void doremir_atomic_modify(doremir_atomic_t a,
                           doremir_atomic_updater_t f)
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


