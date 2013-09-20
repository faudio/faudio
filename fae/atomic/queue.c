
/*
    FA
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/atomic/queue.h>
#include <fa/atomic.h>
#include <fa/string.h>
#include <fa/util.h>

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

struct _fa_atomic_queue_t {
    impl_t      impl;               //  Interface dispatcher
    atomic_t    first, div, last;   //  Node refs
};

fa_ptr_t atomic_queue_impl(fa_id_t interface);

static inline node_t new_node()
{
    return fa_new_struct(node);
}

static inline void delete_node(node_t node)
{
    fa_delete(node);
}

static inline atomic_queue_t new_queue()
{
    atomic_queue_t queue = fa_new(atomic_queue);

    queue->impl  = &atomic_queue_impl;
    queue->first = atomic();
    queue->div   = atomic();
    queue->last  = atomic();

    return queue;
}

static inline void delete_queue(atomic_queue_t queue)
{
    fa_delete(queue->first);
    fa_delete(queue->div);
    fa_delete(queue->last);
    fa_delete(queue);
}

/** Atomically get the node from a place.
 */
static inline node_t get_node(atomic_t place)
{
    return (node_t) fa_atomic_get(place);
}

/** Atomically set a place to a node.
 */
static inline void set_node(atomic_t place, node_t node)
{
    fa_atomic_set(place, node);
}

/** Atomically forward a place to point to the next node.
 */
static inline void forward_node(atomic_t place)
{
    fa_atomic_exchange(place, get_node(place), (get_node(place))->next);
}

/** Non-atomically delete [begin,end)
 */
static inline void delete_range(atomic_t begin, atomic_t end)
{
    while (get_node(begin) != get_node(end)) {
        node_t node = get_node(begin);
        forward_node(begin);
        delete_node(node);
    }
}

/** Non-atomically delete [begin,end]
 */
static inline void delete_range_end(atomic_t begin, atomic_t end)
{
    delete_range(begin, end);
    delete_node(get_node(end));
}

// --------------------------------------------------------------------------------

/** Create a new queue.
    @par Atomicity
        Non-atomic
 */
fa_atomic_queue_t fa_atomic_queue_create()
{
    atomic_queue_t queue = new_queue();
    node_t node          = new_node();

    set_node(queue->first, node);
    set_node(queue->div,   node);
    set_node(queue->last,  node);

    return queue;
}

/** Destroy the given queue.
    @par Atomicity
        Non-atomic
 */
void fa_atomic_queue_destroy(fa_atomic_queue_t queue)
{
    delete_range_end(queue->first, queue->last);
    delete_queue(queue);
}


// --------------------------------------------------------------------------------

/** Write the given value to the given queue.
    @param queuer   Queue.
    @param value    Value to write (optional).
    @par Atomicity
        Atomic
 */
bool fa_atomic_queue_write(fa_atomic_queue_t queue, fa_ptr_t value)
{
    if (!value) {
        return true;
    }

    delete_range(queue->first, queue->div);

    get_node(queue->last)->value = value;
    get_node(queue->last)->next  = new_node();

    forward_node(queue->last);
    return true;
}

/** Read a value from the given queue.
    @return
        A value (optional).
    @par Atomicity
        Atomic
 */
fa_ptr_t fa_atomic_queue_read(fa_atomic_queue_t queue)
{
    ptr_t value;

    if (get_node(queue->div) == get_node(queue->last)) {
        return NULL;
    } else {
        value = get_node(queue->div)->value;
        forward_node(queue->div);
        return value;
    }
}

// --------------------------------------------------------------------------------

fa_string_t atomic_queue_show(fa_ptr_t v)
{
    string_t s = string("<AtomicQueue ");
    s = string_dappend(s, format_integral("%p", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void atomic_queue_destroy(fa_ptr_t a)
{
    fa_atomic_queue_destroy(a);
}


fa_ptr_t atomic_queue_impl(fa_id_t interface)
{
    static fa_string_show_t atomic_queue_show_impl = { atomic_queue_show };
    static fa_destroy_t atomic_queue_destroy_impl = { atomic_queue_destroy };

    switch (interface) {
    case fa_string_show_i:
        return &atomic_queue_show_impl;

    case fa_destroy_i:
        return &atomic_queue_destroy_impl;

    default:
        return NULL;
    }
}

