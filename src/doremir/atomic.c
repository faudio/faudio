
#include <doremir/atomic.h>

struct _doremir_atomic_t
{
};

doremir_atomic_t doremir_atomic_create()
{
}

doremir_atomic_t doremir_atomic_copy(doremir_atomic_t a)
{
}

void doremir_atomic_swap(doremir_atomic_t a, doremir_atomic_t b)
{
}

void doremir_atomic_destroy(doremir_atomic_t a)
{
}

intptr_t doremir_atomic_get(doremir_atomic_t a)
{
}

void doremir_atomic_set(doremir_atomic_t a, intptr_t v)
{
}

void doremir_atomic_modify(doremir_atomic_t a,
                           doremir_atomic_updater_t f)
{
}

