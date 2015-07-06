
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/priority_queue.h>
#include <fa/util.h>

/*
    ## Notes

    * This is a mutable version of the skew heap, see
      <http://en.wikipedia.org/wiki/Skew_heap>

    * Ordering is fixed to fa_less_than at the moment

        - We may want to optimize by adding special mergeBy, insertBy etc.
 */

typedef struct node            *node_t;
typedef fa_priority_queue_t    queue_t;

struct node {
    fa_ptr_t        value;          // Value
    node_t          left;           // Children
    node_t          right;
};

struct _fa_priority_queue_t {
    fa_impl_t       impl;           // Dispatcher
    node_t          node;
};

// -----------------------------------------------------------------------------

inline static node_t new_node(fa_ptr_t value, node_t left, node_t right)
{
    node_t node = fa_new_struct(node);
    node->value = value;
    node->left  = left;
    node->right = right;
    return node;
}

inline static void delete_node(node_t node)
{
    fa_delete(node);
}

fa_ptr_t priority_queue_impl(fa_id_t interface);

inline static queue_t new_queue(node_t node)
{
    queue_t queue = fa_new(priority_queue);
    queue->impl = &priority_queue_impl;
    queue->node = node;
    return queue;
}

inline static void delete_queue(queue_t queue)
{
    fa_delete(queue);
}


// -----------------------------------------------------------------------------

queue_t fa_priority_queue_empty()
{
    return new_queue(NULL);
}

queue_t fa_priority_queue_single(fa_ptr_t value)
{
    return new_queue(new_node(value, NULL, NULL));
}

void fa_priority_queue_destroy(queue_t queue)
{
    delete_node(queue->node);
    delete_queue(queue);
}

static inline node_t merge(node_t node1, node_t node2);
static inline node_t into(node_t node1, node_t node2);

static inline node_t into(node_t node1, node_t node2)
{
    node_t tmp   = node1->left;
    node1->left  = merge(node2, node1->right);
    node1->right = tmp;
    return node1;
}

static inline node_t merge(node_t node1, node_t node2)
{
    if (!node1) {
        return node2;
    } else if (!node2) {
        return node1;
    } else {
        if (fa_less_than_equal(node1->value, node2->value)) {
            return into(node1, node2);
        } else {
            return into(node2, node1);
        }
    }
}

void fa_priority_queue_merge(queue_t queue1, queue_t queue2)
{
    queue1->node = merge(queue1->node, queue2->node);
    delete_queue(queue2);
}

void fa_priority_queue_insert(fa_ptr_t value, queue_t queue)
{
    queue->node = merge(queue->node, new_node(value, NULL, NULL));
}

fa_ptr_t fa_priority_queue_peek(queue_t queue)
{
    node_t head = queue->node;
    return head ? head->value : NULL;
}

fa_ptr_t fa_priority_queue_pop(queue_t queue)
{
    node_t head = queue->node;

    if (!head) {
        return NULL;
    } else {
        fa_ptr_t value = head->value;
        queue->node = merge(head->left, head->right);
        delete_node(head);
        return value;
    }
}

// --------------------------------------------------------------------------------

bool priority_queue_equal(fa_ptr_t a, fa_ptr_t b)
{
    return a == b;
}

fa_string_t priority_queue_show(fa_ptr_t v)
{
    fa_string_t s = fa_string("<PriorityQueue");
    s = fa_string_dappend(s, fa_string_format_integral(" %02x", (long) v));
    s = fa_string_dappend(s, fa_string(">"));
    return s;
}

void priority_queue_destroy(fa_ptr_t a)
{
    fa_priority_queue_destroy(a);
}

void priority_queue_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    // TODO
    
    fa_mark_used(p);
    fa_priority_queue_destroy(a);
}

fa_ptr_t priority_queue_impl(fa_id_t interface)
{
    static fa_equal_t priority_queue_equal_impl = { priority_queue_equal };
    static fa_string_show_t priority_queue_show_impl = { priority_queue_show };
    static fa_destroy_t priority_queue_destroy_impl = { priority_queue_destroy, priority_queue_deep_destroy };

    switch (interface) {
    case fa_equal_i:
        return &priority_queue_equal_impl;

    case fa_string_show_i:
        return &priority_queue_show_impl;

    case fa_destroy_i:
        return &priority_queue_destroy_impl;

    default:
        return NULL;
    }
}

