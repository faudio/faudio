
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/atomic/stack.h>
#include <fa/atomic.h>
#include <fa/string.h>
#include <fa/util.h>

/*
    Notes:
        * Simple unbounded LIFO
        * Does not support multi-read or multi-write
        * All malloc/free is done in writer thread

    Possibilities:
        * Multi read/write
        * Real-time allocator
 */

struct node {
    struct node         *next;
    fa_ptr_t            value;
};

typedef struct node *node_t;

struct _fa_atomic_stack_t {
    fa_impl_t           impl;               //  Interface dispatcher
    fa_atomic_t         top;
};

fa_ptr_t atomic_stack_impl(fa_id_t interface);

static inline node_t new_node(fa_ptr_t value, node_t next)
{
    node_t node = fa_new_struct(node);
    node->value = value;
    node->next  = next;
    return node;
}

static inline void delete_node(node_t node)
{
    fa_delete(node);
}

static inline fa_atomic_stack_t new_stack()
{
    fa_atomic_stack_t stack = fa_new(atomic_stack);

    stack->impl  = &atomic_stack_impl;
    stack->top   = fa_atomic();

    return stack;
}

static inline void delete_stack(fa_atomic_stack_t stack)
{
    fa_delete(stack->top);
    fa_delete(stack);
}

/** Atomically get the node from a place.
 */
static inline node_t get_node(fa_atomic_t place)
{
    return (node_t) fa_atomic_get(place);
}

/** Atomically set a place to a node.
 */
static inline void set_node(fa_atomic_t place, node_t node)
{
    fa_atomic_set(place, node);
}

/** Atomically forward a place to point to the next node.
 */
static inline void forward_node(fa_atomic_t place)
{
    fa_atomic_exchange(place, get_node(place), (get_node(place))->next);
}

// --------------------------------------------------------------------------------

fa_atomic_stack_t fa_atomic_stack_create()
{
    fa_atomic_stack_t stack = new_stack();
    return stack;
}

void fa_atomic_stack_destroy(fa_atomic_stack_t stack)
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

bool fa_atomic_stack_write(fa_atomic_stack_t stack, fa_ptr_t value)
{
    node_t node, node2;

    do {
        node = get_node(stack->top);
        node2 = new_node(value, node);
    } while (!fa_atomic_exchange(stack->top, node, node2));

    return true;
}

fa_ptr_t fa_atomic_stack_read(fa_atomic_stack_t stack)
{
    node_t node;

    do {
        node = get_node(stack->top);

        if (!node) {
            return false;
        }
    } while (!fa_atomic_exchange(stack->top, node, node->next));

    fa_ptr_t value = node->value;
    delete_node(node);
    return value;
}

// --------------------------------------------------------------------------------

fa_string_t atomic_stack_show(fa_ptr_t v)
{
    fa_string_t s = fa_string("<AtomicStack ");
    s = fa_string_dappend(s, fa_format_integral("%p", (long) v));
    s = fa_string_dappend(s, fa_string(">"));
    return s;
}

void atomic_stack_destroy(fa_ptr_t a)
{
    fa_atomic_stack_destroy(a);
}


fa_ptr_t atomic_stack_impl(fa_id_t interface)
{
    static fa_string_show_t atomic_stack_show_impl = { atomic_stack_show };
    static fa_destroy_t atomic_stack_destroy_impl = { atomic_stack_destroy };

    switch (interface) {
    case fa_string_show_i:
        return &atomic_stack_show_impl;

    case fa_destroy_i:
        return &atomic_stack_destroy_impl;

    default:
        return NULL;
    }
}

