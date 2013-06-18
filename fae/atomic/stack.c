
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/atomic/stack.h>
#include <fae/atomic.h>
#include <fae/string.h>
#include <fae/util.h>

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
    struct node    *next;
    ptr_t           value;
};

typedef struct node *node_t;

struct _fae_atomic_stack_t {
    impl_t      impl;               //  Interface dispatcher
    atomic_t    top;
};

fae_ptr_t atomic_stack_impl(fae_id_t interface);

static inline node_t new_node(ptr_t value, node_t next)
{
    node_t node = fae_new_struct(node);
    node->value = value;
    node->next  = next;
    return node;
}

static inline void delete_node(node_t node)
{
    fae_delete(node);
}

static inline fae_atomic_stack_t new_stack()
{
    atomic_stack_t stack = fae_new(atomic_stack);

    stack->impl  = &atomic_stack_impl;
    stack->top   = atomic();

    return stack;
}

static inline void delete_stack(atomic_stack_t stack)
{
    fae_delete(stack->top);
    fae_delete(stack);
}

/** Atomically get the node from a place.
 */
static inline node_t get_node(atomic_t place)
{
    return (node_t) fae_atomic_get(place);
}

/** Atomically set a place to a node.
 */
static inline void set_node(atomic_t place, node_t node)
{
    fae_atomic_set(place, node);
}

/** Atomically forward a place to point to the next node.
 */
static inline void forward_node(atomic_t place)
{
    fae_atomic_exchange(place, get_node(place), (get_node(place))->next);
}

// --------------------------------------------------------------------------------

/** Create a new stack.
    @par Atomicity
        Non-atomic
 */
fae_atomic_stack_t fae_atomic_stack_create()
{
    atomic_stack_t stack = new_stack();
    return stack;
}

/** Destroy the given stack.
    @par Atomicity
        Non-atomic
 */
void fae_atomic_stack_destroy(fae_atomic_stack_t stack)
{
    while (true) {
        node_t node = get_node(stack->top);

        if (!node) {
            break;
        }

        forward_node(stack->top);
        delete_node(node);
    }

    delete_stack(stack);
}


// --------------------------------------------------------------------------------

/** Write the given value to the given stack.
    @param stackr   Queue.
    @param value    Value to write (optional).
    @par Atomicity
        Atomic
 */
bool fae_atomic_stack_write(fae_atomic_stack_t stack, fae_ptr_t value)
{
    node_t node, node2;

    do {
        node = get_node(stack->top);
        node2 = new_node(value, node);
    } while (!fae_atomic_exchange(stack->top, node, node2));

    return true;
}

/** Read a value from the given stack.
    @return
        A value (optional).
    @par Atomicity
        Atomic
 */
fae_ptr_t fae_atomic_stack_read(fae_atomic_stack_t stack)
{
    node_t node;

    do {
        node = get_node(stack->top);

        if (!node) {
            return false;
        }
    } while (!fae_atomic_exchange(stack->top, node, node->next));

    ptr_t value = node->value;
    delete_node(node);
    return value;
}

// --------------------------------------------------------------------------------

fae_string_t atomic_stack_show(fae_ptr_t v)
{
    string_t s = string("<AtomicStack ");
    s = string_dappend(s, format_integral("%p", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void atomic_stack_destroy(fae_ptr_t a)
{
    fae_atomic_stack_destroy(a);
}


fae_ptr_t atomic_stack_impl(fae_id_t interface)
{
    static fae_string_show_t atomic_stack_show_impl = { atomic_stack_show };
    static fae_destroy_t atomic_stack_destroy_impl = { atomic_stack_destroy };

    switch (interface) {
    case fae_string_show_i:
        return &atomic_stack_show_impl;

    case fae_destroy_i:
        return &atomic_stack_destroy_impl;

    default:
        return NULL;
    }
}

