
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/atomic/queue.h>
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

struct _doremir_atomic_queue_t {
        impl_t      impl;               //  Interface dispatcher
        atomic_t    first, div, last;   //  Node refs
    };

doremir_ptr_t atomic_queue_impl(doremir_id_t interface);

static inline
node_t new_node()
{
    return doremir_new_struct(node);
}

static inline
void delete_node(node_t node)
{
    doremir_delete(node);
}

static inline
atomic_queue_t new_queue()
{
    atomic_queue_t queue = doremir_new(atomic_queue);

    queue->impl  = &atomic_queue_impl;
    queue->first = atomic();
    queue->div   = atomic();
    queue->last  = atomic();

    return queue;
}

static inline
void delete_queue(atomic_queue_t queue)
{
    doremir_delete(queue->first);
    doremir_delete(queue->div);
    doremir_delete(queue->last);

    doremir_delete(queue);
}

/** Atomically get the node from a place.
 */
static inline
node_t get_node(atomic_t place)
{
    return (node_t) doremir_atomic_get(place);
}

/** Atomically set a place to a node.
 */
static inline
void set_node(atomic_t place, node_t node)
{
    doremir_atomic_set(place, node);
}

/** Atomically forward a place to point to the next node.
 */
#define forward_node(place) \
    doremir_atomic_exchange(place, get_node(place), (get_node(place))->next);

/** Non-atomically delete [begin,end)
 */
static inline
void delete_range(atomic_t begin, atomic_t end)
{
    while (get_node(begin) != get_node(end))
    {
        node_t node = get_node(begin);
        forward_node(begin);
        delete_node(node);
    }
}

/** Non-atomically delete [begin,end]
 */
static inline
void delete_range_end(atomic_t begin, atomic_t end)
{
    delete_range(begin, end);
    delete_node(get_node(end));
}

// --------------------------------------------------------------------------------

doremir_atomic_queue_t doremir_atomic_queue_create()
{
    atomic_queue_t queue = new_queue();
    node_t         node  = new_node(NULL);

    set_node(queue->first, node);
    set_node(queue->div,   node);
    set_node(queue->last,  node);

    return queue;
}

void doremir_atomic_queue_destroy(doremir_atomic_queue_t queue)
{
    delete_range_end(queue->first, queue->last);
    delete_queue(queue);
}


// --------------------------------------------------------------------------------

bool doremir_atomic_queue_write(doremir_atomic_queue_t queue, doremir_ptr_t value)
{
    delete_range(queue->first, queue->div);

    get_node(queue->last)->value = value;
    get_node(queue->last)->next  = new_node(NULL);

    forward_node(queue->last);

    return true;
}

doremir_ptr_t doremir_atomic_queue_read(doremir_atomic_queue_t queue)
{
    ptr_t value;

    if (get_node(queue->div) == get_node(queue->last))
        return NULL;
    else
    {
        value = get_node(queue->div)->value;
        forward_node(queue->div);
        return value;
    }
}

// --------------------------------------------------------------------------------

doremir_string_t atomic_queue_show(doremir_ptr_t v)
{
    string_t s = string("<AtomicQueue ");
    s = string_dappend(s, format_int("%p", (long) v));
    s = string_dappend(s, string(">"));
    return s;
}

void atomic_queue_destroy(doremir_ptr_t a)
{
    doremir_atomic_queue_destroy(a);
}


doremir_ptr_t atomic_queue_impl(doremir_id_t interface)
{
    static doremir_string_show_t atomic_queue_show_impl = { atomic_queue_show };
    static doremir_destroy_t atomic_queue_destroy_impl = { atomic_queue_destroy };

    switch (interface)
    {
    case doremir_string_show_i:
        return &atomic_queue_show_impl;

    case doremir_destroy_i:
        return &atomic_queue_destroy_impl;

    default:
        return NULL;
    }
}

