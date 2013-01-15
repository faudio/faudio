
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/atomic/stack.h>
#include <doremir/atomic.h>
#include <doremir/string.h>
#include <doremir/util.h>

/*
    Notes:
        * Simple unbounded FIFO
        * Does not support multi-read or multi-write
        * All malloc/free is done in writer thread
        
    Possibilities:
        * Multi read/write
        * Real-time allocator
 */

struct node {
        atomic_t    next;
        ptr_t       value;
    };

typedef struct node * node_t;

struct _doremir_atomic_stack_t {
        impl_t      impl;               //  Interface dispatcher
    };

doremir_ptr_t atomic_stack_impl(doremir_id_t interface);

static inline node_t new_node()
{
    return doremir_new_struct(node);
}

static inline void delete_node(node_t node)
{
    doremir_delete(node);
}

static inline doremir_atomic_stack_t new_stack()
{
    atomic_stack_t stack = doremir_new(atomic_stack);

    stack->impl  = &atomic_stack_impl;
    // TODO
    return stack;
}

static inline void delete_stack(atomic_stack_t stack)
{
    // TODO
    doremir_delete(stack);
}


// --------------------------------------------------------------------------------

doremir_atomic_stack_t doremir_atomic_stack_create()
{
    atomic_stack_t stack = new_stack();
    // TODO
    return stack;
}

void doremir_atomic_stack_destroy(doremir_atomic_stack_t stack)
{
    // TODO
    delete_stack(stack);
}


// --------------------------------------------------------------------------------

bool doremir_atomic_stack_write(doremir_atomic_stack_t stack, doremir_ptr_t value)
{
    // TODO
    return true;
}

doremir_ptr_t doremir_atomic_stack_read(doremir_atomic_stack_t stack)
{
    // TODO
    return NULL;
}

// --------------------------------------------------------------------------------

doremir_string_t atomic_stack_show(doremir_ptr_t v)
{
    string_t s = string("<AtomicStack ");
    s = string_dappend(s, format_int("%p", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void atomic_stack_destroy(doremir_ptr_t a)
{
    doremir_atomic_stack_destroy(a);
}


doremir_ptr_t atomic_stack_impl(doremir_id_t interface)
{
    static doremir_string_show_t atomic_stack_show_impl = { atomic_stack_show };
    static doremir_destroy_t atomic_stack_destroy_impl = { atomic_stack_destroy };

    switch (interface)
    {
    case doremir_string_show_i:
        return &atomic_stack_show_impl;

    case doremir_destroy_i:
        return &atomic_stack_destroy_impl;

    default:
        return NULL;
    }
}

