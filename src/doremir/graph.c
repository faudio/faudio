
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/graph.h>
#include <doremir/set.h>
#include <doremir/string.h>
#include <doremir/util.h>

/*
    Notes:
 */

struct edge {
    impl_t      impl;       //  Interface dispatcher
    // label
    // node1
    // node2
};

typedef struct edge * edge_t;

struct _doremir_graph_t {
    impl_t          impl;       //  Interface dispatcher
    set_t           entries;    //  Set of entries
};

doremir_ptr_t graph_impl(doremir_id_t interface);
doremir_ptr_t edge_impl(doremir_id_t interface);

// edge_t new_edge(doremir_ptr_t key, doremir_ptr_t value)
// {
//   edge_t edge   = doremir_new_struct(edge);
//   edge->impl     = &edge_impl;
//   edge->key      = key;
//   edge->value    = value;
//   return edge;
// }
//
// edge_t copy_edge(edge_t edge)
// {
//   return new_edge(edge->key, edge->value);
// }
//
// void delete_edge(pair_t edge)
// {
//   doremir_delete(edge);
// }
//
// inline static graph_t new_graph(set_t entries)
// {
//   graph_t graph       = doremir_new(graph);
//   graph->impl       = &graph_impl;
//   graph->entries    = entries;
//   return graph;
// }
//
// inline static void
// delete_graph(graph_t graph)
// {
//   doremir_delete(graph);
// }


// --------------------------------------------------------------------------------

/** Create an empty graph.
 */
doremir_graph_t doremir_graph_empty()
{
    assert(false && "Not implemented.");
}

/** Insert a node in the given graph.
 */
doremir_graph_t doremir_graph_insert(doremir_graph_node_t node,
                                     doremir_graph_t graph)
{
    assert(false && "Not implemented.");
}

/** Remove a node from the given graph.
 */
doremir_graph_t doremir_graph_remove(doremir_graph_node_t node,
                                     doremir_graph_t graph)
{
    assert(false && "Not implemented.");
}

/** Destroy the given graph.
 */
void doremir_graph_destroy(doremir_graph_t graph)
{
    assert(false && "Not implemented.");
}

/** Connect the given nodes.
 */
doremir_graph_t doremir_graph_connect(doremir_graph_edge_t edge,
                                      doremir_graph_node_t node1,
                                      doremir_graph_node_t node2,
                                      doremir_graph_t graph)
{
    assert(false && "Not implemented.");
}

/** Remove all connections between the given nodes.
 */
doremir_graph_t doremir_graph_disconnect(doremir_graph_node_t node1,
        doremir_graph_node_t node2,
        doremir_graph_t graph)
{
    assert(false && "Not implemented.");
}

doremir_graph_t doremir_graph_from_list(doremir_list_t list)
{
    assert(false && "Not implemented.");
}

doremir_list_t doremir_graph_to_list(doremir_graph_t graph)
{
    assert(false && "Not implemented.");
}



// --------------------------------------------------------------------------------

// bool edge_equal(doremir_ptr_t a, doremir_ptr_t b)
// {
//   edge_t c = (edge_t) a;
//   edge_t d = (edge_t) b;
//   return doremir_equal(c->key, d->key);
// }
//
// bool edge_less_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//   edge_t c = (edge_t) a;
//   edge_t d = (edge_t) b;
//   return doremir_less_than(c->key, d->key);
// }
//
// bool edge_greater_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//   edge_t c = (edge_t) a;
//   edge_t d = (edge_t) b;
//   return doremir_greater_than(c->key, d->key);
// }
//
// doremir_string_t edge_show(doremir_ptr_t a)
// {
//   edge_t b = (edge_t) a;
//   string_t s = string("<Entry (");
//   s = string_dappend(s, doremir_string_show(b->key));
//   s = string_dappend(s, string(","));
//   s = string_dappend(s, doremir_string_show(b->value));
//   s = string_dappend(s, string(")>"));
//   return s;
// }
//
// doremir_ptr_t edge_copy(doremir_ptr_t a)
// {
//   return copy_edge(a);
// }
//
// void edge_destroy(doremir_ptr_t a)
// {
//   delete_edge(a);
// }
//
// doremir_ptr_t edge_impl(doremir_id_t interface)
// {
//   static doremir_equal_t edge_equal_impl = { edge_equal };
//   static doremir_order_t edge_order_impl = { edge_less_than, edge_greater_than };
//   static doremir_string_show_t edge_show_impl = { edge_show };
//   static doremir_copy_t edge_copy_impl = { edge_copy };
//   static doremir_destroy_t edge_destroy_impl = { edge_destroy };
//
//   switch (interface) {
//   case doremir_equal_i:
//     return &edge_equal_impl;
//
//   case doremir_order_i:
//     return &edge_order_impl;
//
//   case doremir_string_show_i:
//     return &edge_show_impl;
//
//   case doremir_copy_i:
//     return &edge_copy_impl;
//
//   case doremir_destroy_i:
//     return &edge_destroy_impl;
//
//   default:
//     return NULL;
//   }
// }


// --------------------------------------------------------------------------------

// bool graph_equal(doremir_ptr_t a, doremir_ptr_t b)
// {
//   graph_t c = (graph_t) a;
//   graph_t d = (graph_t) b;
//   return doremir_equal(c->entries, d->entries);
// }
//
// bool graph_less_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//   graph_t c = (graph_t) a;
//   graph_t d = (graph_t) b;
//   return doremir_less_than(c->entries, d->entries);
// }
//
// bool graph_greater_than(doremir_ptr_t a, doremir_ptr_t b)
// {
//   graph_t c = (graph_t) a;
//   graph_t d = (graph_t) b;
//   return doremir_greater_than(c->entries, d->entries);
// }
//
// doremir_string_t graph_show(doremir_ptr_t x)
// {
//   graph_t graph = (graph_t) x;
//   string_t s  = string("{");
//
//   doremir_for_each_last(x, doremir_set_to_list(graph->entries), last) {
//     edge_t edge = x;
//     s = string_dappend(s, doremir_string_show(edge->key));
//     s = string_dappend(s, string(": "));
//     s = string_dappend(s, doremir_string_show(edge->value));
//
//     if (!last) {
//       s = string_dappend(s, string(", "));
//     }
//   }
//   s = string_dappend(s, string("}"));
//   return s;
// }
//
// doremir_ptr_t graph_copy(doremir_ptr_t a)
// {
//   return doremir_graph_copy(a);
// }
//
// void graph_destroy(doremir_ptr_t a)
// {
//   doremir_graph_destroy(a);
// }
//
// doremir_ptr_t graph_impl(doremir_id_t interface)
// {
//   static doremir_equal_t graph_equal_impl = { graph_equal };
//   static doremir_string_show_t graph_show_impl = { graph_show };
//   static doremir_copy_t graph_copy_impl = { graph_copy };
//   static doremir_destroy_t graph_destroy_impl = { graph_destroy };
//
//   switch (interface) {
//   case doremir_equal_i:
//     return &graph_equal_impl;
//
//   case doremir_string_show_i:
//     return &graph_show_impl;
//
//   case doremir_copy_i:
//     return &graph_copy_impl;
//
//   case doremir_destroy_i:
//     return &graph_destroy_impl;
//
//   default:
//     return NULL;
//   }
// }
//
