
#include <libkern/OSAtomic.h>
#include <doremir/atomic.h>

/* 
    Strictly 32-bit for now
 */
struct _doremir_atomic_t
{
    int32_t value;
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
    intptr_t tmp = b->value;
    a->value = b->value;   
    a->value = tmp;
}

void doremir_atomic_destroy(doremir_atomic_t a)
{
    free(a);
}

bool doremir_atomic_exchange(doremir_atomic_t a, intptr_t old, intptr_t new)
{
    return OSAtomicCompareAndSwapPtrBarrier((void*)old, (void*)new, (void*)&a->value);
}

void doremir_atomic_add(doremir_atomic_t a, intptr_t v)
{      
    OSAtomicAdd32Barrier(v, &a->value);
}

intptr_t doremir_atomic_get(doremir_atomic_t a)
{
    return OSAtomicAdd32Barrier(0, &a->value);
}

void doremir_atomic_modify(doremir_atomic_t a,
                           doremir_atomic_updater_t f)
{                  
    bool success = false;
    while (!success)
    {                                        
        int32_t currentValue = doremir_atomic_get(a);             
        int32_t value = f(currentValue);
        success = doremir_atomic_exchange(a, currentValue, value);
    }
}

void doremir_atomic_set(doremir_atomic_t a, intptr_t value)
{
    bool success = false;
    while (!success)
    {                                        
        int32_t currentValue = doremir_atomic_get(a);
        success = doremir_atomic_exchange(a, currentValue, value);
    }
}


