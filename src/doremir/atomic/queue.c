
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
        
        doremir_atomic_t    first;
        doremir_atomic_t    div;
        doremir_atomic_t    last;
};

doremir_ptr_t atomic_queue_impl(doremir_id_t interface);

// doremir_atomic_queue_t new_queue()
// {
// }
// 
// void delete_queue(doremir_atomic_queue_t queue)
// {
//     doremir_delete(queue);
// }

// --------------------------------------------------------------------------------

doremir_atomic_queue_t doremir_atomic_queue_create()
{
    doremir_atomic_queue_t q = doremir_new(atomic_queue);
    q->impl = &atomic_queue_impl;
    
    q->first = doremir_new_struct(node);
    q->div   = q->first;
    q->last  = q->first;
    return q;
}

void doremir_atomic_queue_destroy(doremir_atomic_queue_t q)
{                                                  
    while (doremir_atomic_get(q->first) != doremir_atomic_get(q->last))
    {
        node_t r = doremir_atomic_get(q->first);
        doremir_atomic_exchange(q->first, r, doremir_atomic_get(r->next));
        doremir_delete(r);
    }
}

bool doremir_atomic_queue_write(doremir_atomic_queue_t q, doremir_ptr_t v)
{
    while (doremir_atomic_get(q->first) != doremir_atomic_get(q->div))
    {
        node_t r = doremir_atomic_get(q->first);
        doremir_atomic_exchange(q->first, r, doremir_atomic_get(r->next));
        doremir_delete(r);
    }
    
    ((node_t) doremir_atomic_get(q->last))->value = v;
    ((node_t) doremir_atomic_get(q->last))->next  = doremir_new_struct(node);
    
    doremir_atomic_exchange(q->last, doremir_atomic_get(q->last), ((node_t) doremir_atomic_get(q->last))->next);
    return true;
}

doremir_ptr_t doremir_atomic_queue_read(doremir_atomic_queue_t q)
{
    if (doremir_atomic_get(q->div) == doremir_atomic_get(q->last))
        return NULL;
    else
    {
        doremir_ptr_t v = ((node_t) doremir_atomic_get(q->div))->value;
        doremir_atomic_exchange(q->div, doremir_atomic_get(q->div), ((node_t) doremir_atomic_get(q->div))->next);
        return v;
    }
}

// --------------------------------------------------------------------------------

doremir_string_t atomic_queue_show(doremir_ptr_t v)
{
    string_t s = string("<AtomicQueue ");
    s = sdappend(s, doremir_string_format_integer("%p", (int) v));
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

