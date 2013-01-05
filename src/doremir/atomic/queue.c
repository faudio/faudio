
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/atomic/queue.h>
#include <doremir/atomic.h>
#include <doremir/string.h>
#include <doremir/util.h>

struct node {
        doremir_atomic_t    next;
        doremir_ptr_t       value;
};

typedef struct node * node_t;

struct _doremir_atomic_queue_t {

        doremir_impl_t      impl;       /* Interface dispatcher */

        doremir_atomic_t    div;        /* (first,last) is entire queue queue */
        doremir_atomic_t    first;      /* (first,div)  is owned by writer */
        doremir_atomic_t    last;       /* (div,last)   is owned by reader */
};

doremir_ptr_t atomic_queue_impl(doremir_id_t interface);

#define get_node(N) \
    ((node_t) doremir_atomic_get(N))

// TODO does not need to be exchange?
#define forward_node(N) \
    doremir_atomic_exchange(N, doremir_atomic_get(N), (get_node(N))->next);
    

// --------------------------------------------------------------------------------

doremir_atomic_queue_t doremir_atomic_queue_create()
{
    doremir_atomic_queue_t queue = doremir_new(atomic_queue);
    node_t                 node = doremir_new_struct(node);

    queue->impl = &atomic_queue_impl;
    
    queue->first = doremir_atomic_create();
    queue->div   = doremir_atomic_create();
    queue->last  = doremir_atomic_create();          
                                     
    doremir_atomic_set(queue->first, node);
    doremir_atomic_set(queue->div,   node);
    doremir_atomic_set(queue->last,  node);
    return queue;
}

void doremir_atomic_queue_destroy(doremir_atomic_queue_t queue)
{                                                  
    while (get_node(queue->first) != get_node(queue->last))
    {
        node_t node = get_node(queue->first);
        forward_node(queue->first);
        doremir_delete(node);
    }              
    doremir_delete(get_node(queue->last));
    doremir_delete(queue);    
}

bool doremir_atomic_queue_write(doremir_atomic_queue_t queue, doremir_ptr_t value)
{
    while (get_node(queue->first) != get_node(queue->div))
    {
        node_t node = get_node(queue->first);
        forward_node(queue->first);
        doremir_delete(node);
    }
    
    get_node(queue->last)->value = value;
    get_node(queue->last)->next  = doremir_new_struct(node);
    
    forward_node(queue->last);
    return true;
}

doremir_ptr_t doremir_atomic_queue_read(doremir_atomic_queue_t queue)
{
    if (get_node(queue->div) == get_node(queue->last))
        return NULL;
    else
    {
        doremir_ptr_t v = get_node(queue->div)->value;
        forward_node(queue->div);
        return v;
    }
}

// --------------------------------------------------------------------------------

doremir_string_t atomic_queue_show(doremir_ptr_t v)
{
    string_t s = string("<AtomicQueue ");
    s = sdappend(s, format_int("%p", (long) v));
    s = sdappend(s, string(">"));
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

