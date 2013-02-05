
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
    struct node    *next;
    ptr_t           value;
};

typedef struct node *node_t;

struct _doremir_atomic_stack_t {
    impl_t      impl;               //  Interface dispatcher
    atomic_t    top;
};

doremir_ptr_t atomic_stack_impl(doremir_id_t interface);

static inline node_t new_node(ptr_t value, node_t next)
{
    node_t node = doremir_new_struct(node);
    node->value = value;
    node->next  = next;
    return node;
}

static inline void delete_node(node_t node)
{
    doremir_delete(node);
}

static inline doremir_atomic_stack_t new_stack()
{
    atomic_stack_t stack = doremir_new(atomic_stack);

    stack->impl  = &atomic_stack_impl;
    stack->top   = atomic();

    return stack;
}

static inline void delete_stack(atomic_stack_t stack)
{
    doremir_delete(stack->top);
    doremir_delete(stack);
}

/** Atomically get the node from a place.
 */
static inline node_t get_node(atomic_t place)
{
    return (node_t) doremir_atomic_get(place);
}

/** Atomically set a place to a node.
 */
static inline void set_node(atomic_t place, node_t node)
{
    doremir_atomic_set(place, node);
}

/** Atomically forward a place to point to the next node.
 */
static inline void forward_node(atomic_t place)
{
    doremir_atomic_exchange(place, get_node(place), (get_node(place))->next);
}

// --------------------------------------------------------------------------------

/** Create a new stack.
    @par Atomicity
        Non-atomic
 */
doremir_atomic_stack_t doremir_atomic_stack_create()
{
    atomic_stack_t stack = new_stack();
    return stack;
}

/** Destroy the given stack.
    @par Atomicity
        Non-atomic
 */
void doremir_atomic_stack_destroy(doremir_atomic_stack_t stack)
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
bool doremir_atomic_stack_write(doremir_atomic_stack_t stack, doremir_ptr_t value)
{
    node_t node, node2;

    do {
        node = get_node(stack->top);
        node2 = new_node(value, node);
    } while (!doremir_atomic_exchange(stack->top, node, node2));

    return true;
}

/** Read a value from the given stack.
    @return
        A value (optional).
    @par Atomicity
        Atomic
 */
doremir_ptr_t doremir_atomic_stack_read(doremir_atomic_stack_t stack)
{
    node_t node;

    do {
        node = get_node(stack->top);

        if (!node) {
            return false;
        }
    } while (!doremir_atomic_exchange(stack->top, node, node->next));

    ptr_t value = node->value;
    delete_node(node);
    return value;
}

// --------------------------------------------------------------------------------

doremir_string_t atomic_stack_show(doremir_ptr_t v)
{
    string_t s = string("<AtomicStack ");
    s = string_dappend(s, format_integer("%p", (long) v));
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

    switch (interface) {
    case doremir_string_show_i:
        return &atomic_stack_show_impl;

    case doremir_destroy_i:
        return &atomic_stack_destroy_impl;

    default:
        return NULL;
    }
}

