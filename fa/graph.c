
/*
    FA
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/graph.h>
#include <fa/set.h>
#include <fa/string.h>
#include <fa/dynamic.h>
#include <fa/util.h>

/*
    Notes:
        FIXME only allows one connection per node pair
 */

struct edge {
    impl_t      impl;           //  Interface dispatcher
    ptr_t       node1;
    ptr_t       node2;
};

typedef ptr_t         node_t;
typedef struct edge *edge_t;

struct _fa_graph_t {
    impl_t          impl;       //  Interface dispatcher
    set_t           nodes;      //  Set n
    map_t           edges;      //  Map (n,n) e
};


fa_ptr_t graph_impl(fa_id_t interface);
fa_ptr_t edge_impl(fa_id_t interface);

edge_t new_edge(ptr_t node1, ptr_t node2)
{
    edge_t edge   = fa_new_struct(edge);
    edge->impl    = &edge_impl;
    edge->node1   = node1;
    edge->node2   = node2;
    return edge;
}

edge_t copy_edge(edge_t edge)
{
    return new_edge(edge->node1, edge->node2);
}

void delete_edge(pair_t edge)
{
    fa_delete(edge);
}

inline static graph_t new_graph(set_t nodes, map_t edges)
{
    graph_t graph     = fa_new(graph);
    graph->impl       = &graph_impl;
    graph->nodes      = nodes;
    graph->edges      = edges;
    return graph;
}

inline static void
delete_graph(graph_t graph)
{
    fa_delete(graph);
}


// --------------------------------------------------------------------------------

/** Create an empty graph.
 */
fa_graph_t fa_graph_empty()
{
    return new_graph(fa_set_empty(), fa_map_empty());
}

/** Insert a node in the given graph.
    @param node     Node to insert.
    @param graph    The graph.
 */
fa_graph_t fa_graph_insert(fa_graph_node_t node,
                             fa_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    return new_graph(fa_set_set(node, nodes), fa_copy(edges));
}

/** Remove a node from the given graph.
    @param node     Node to remove (comparing on equality).
    @param graph    The graph.
 */
fa_graph_t fa_graph_remove(fa_graph_node_t node,
                             fa_graph_t graph)
{
    assert(false && "Not implemented");
    // set_t nodes = graph->nodes;
    // map_t edges = graph->edges;
    // filter out entries not in node set
    // return new_graph(fa_set_remove(node, nodes), fa_copy(edges));
}

/** Copy the given graph.
    @param graph    The graph.
 */
fa_graph_t fa_graph_copy(fa_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    return new_graph(fa_copy(nodes), fa_copy(edges));
}

/** Destroy the given graph.
 */
void fa_graph_destroy(fa_graph_t graph)
{
    fa_destroy(graph->nodes);
    fa_destroy(graph->edges);
    delete_graph(graph);
}

/** Connect the given nodes.
    @param node1    First node in connection.
    @param node2    Second node in connection.
    @param label    Label to attach to connection.
    @param graph    The graph.
 */
fa_graph_t fa_graph_connect(fa_graph_node_t node1,
                              fa_graph_node_t node2,
                              fa_graph_edge_t label,
                              fa_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    edge_t edge = new_edge(node1, node2);

    if (fa_set_has(node1, nodes) && fa_set_has(node2, nodes)) {
        return new_graph(fa_copy(nodes), fa_map_set(edge, label, edges));
    } else {
        return fa_graph_copy(graph);
    }
}

/** Remove all connections between the given nodes.
    @param node1    First node in connection.
    @param node2    Second node in connection.
    @param graph    The graph.
 */
fa_graph_t fa_graph_disconnect(fa_graph_node_t node1,
                                 fa_graph_node_t node2,
                                 fa_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    edge_t edge = new_edge(node1, node2);
    return new_graph(fa_copy(nodes), fa_map_remove(edge, edges));
}


// --------------------------------------------------------------------------------

/** Convert the given graph to a [Dot](http://en.wikipedia.org/wiki/DOT_language)
    declaration.

    @param header           Text to include before graph declaration.
    @param inner_header     Text to include at the top of the graph declaration.
    @param graph            Graph to convert. Its node and edge values will be converted to
                            strings using [show](@ref fa_string_show). It the nodes
                            are pairs, their first component will be used as the node identifier
                            and the second as a label.

 */
fa_string_t fa_graph_to_dot(
    fa_string_t header,
    fa_string_t inner_header,
    fa_graph_t graph)
{
    string_t str = string("\n\n");
    char buf[100];

    str = string_dappend(str, header);
    str = string_dappend(str, string("\n\n"));

    str = string_dappend(str, string("digraph {\n"));

    str = string_dappend(str, inner_header);
    str = string_dappend(str, string("\n\n"));

    fa_set_for_each(node, graph->nodes) {
        if (fa_dynamic_get_type(node) == pair_type_repr) {
            char *cs = unstring(fa_string_to_string(fa_pair_first(node)));
            char *ds = unstring(fa_string_to_string(fa_pair_second(node)));

            snprintf(buf, 100, "    \"%s\" [label = \"%s\"];\n", cs, ds);
            str = string_dappend(str, string(buf));
        } else {
            char *cs = unstring(fa_string_to_string(node));

            snprintf(buf, 100, "    \"%s\";\n", cs);
            str = string_dappend(str, string(buf));
        }
    }
    fa_map_for_each(node_edge_pair, graph->edges) {
        ptr_t node1, node2, label;

        node1 = ((edge_t) fa_pair_first(node_edge_pair))->node1;
        node2 = ((edge_t) fa_pair_first(node_edge_pair))->node2;
        label = fa_pair_second(node_edge_pair);

        if (fa_dynamic_get_type(node1) == pair_type_repr) {
            node1   = fa_pair_first(node1);
            node2   = fa_pair_first(node2);
        }

        char *n1    = unstring(fa_string_to_string(node1));
        char *n2    = unstring(fa_string_to_string(node2));
        char *l     = unstring(fa_string_to_string(label));

        snprintf(buf, 100, "    \"%s\" -> \"%s\" [label=\"%s\"];\n", n1, n2, l);
        str = string_dappend(str, string(buf));
    }
    str = string_dappend(str, string("}\n\n"));
    return str;
}

// --------------------------------------------------------------------------------

bool edge_equal(fa_ptr_t a, fa_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return fa_equal(c->node1, d->node1)
           && fa_equal(c->node2, d->node2);
}

bool edge_less_than(fa_ptr_t a, fa_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return fa_less_than(c->node1, d->node1)
           || (fa_equal(c->node1, d->node1)
               && fa_less_than(c->node2, d->node2));
}

bool edge_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return fa_greater_than(c->node1, d->node1)
           || (fa_equal(c->node1, d->node1)
               && fa_greater_than(c->node2, d->node2));
}

fa_string_t edge_show(fa_ptr_t a)
{
    edge_t b = (edge_t) a;
    string_t s = string("(");
    s = string_dappend(s, fa_string_show(b->node1));
    s = string_dappend(s, string(","));
    s = string_dappend(s, fa_string_show(b->node2));
    s = string_dappend(s, string(")"));
    return s;
}

fa_ptr_t edge_copy(fa_ptr_t a)
{
    return copy_edge(a);
}

void edge_destroy(fa_ptr_t a)
{
    delete_edge(a);
}

fa_ptr_t edge_impl(fa_id_t interface)
{
    static fa_equal_t edge_equal_impl = { edge_equal };
    static fa_order_t edge_order_impl = { edge_less_than, edge_greater_than };
    static fa_string_show_t edge_show_impl = { edge_show };
    static fa_copy_t edge_copy_impl = { edge_copy };
    static fa_destroy_t edge_destroy_impl = { edge_destroy };

    switch (interface) {
    case fa_equal_i:
        return &edge_equal_impl;

    case fa_order_i:
        return &edge_order_impl;

    case fa_string_show_i:
        return &edge_show_impl;

    case fa_copy_i:
        return &edge_copy_impl;

    case fa_destroy_i:
        return &edge_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool graph_equal(fa_ptr_t a, fa_ptr_t b)
{
    graph_t c = (graph_t) a;
    graph_t d = (graph_t) b;
    return fa_equal(c->nodes, d->nodes)
           && fa_equal(c->edges, d->edges);
}

fa_string_t graph_show(fa_ptr_t x)
{
    graph_t graph = (graph_t) x;
    return fa_string_show(pair(graph->nodes, graph->edges)); // TODO dshow
}

fa_ptr_t graph_copy(fa_ptr_t a)
{
    return fa_graph_copy(a);
}

void graph_destroy(fa_ptr_t a)
{
    fa_graph_destroy(a);
}

fa_ptr_t graph_impl(fa_id_t interface)
{
    static fa_equal_t graph_equal_impl = { graph_equal };
    static fa_string_show_t graph_show_impl = { graph_show };
    static fa_copy_t graph_copy_impl = { graph_copy };
    static fa_destroy_t graph_destroy_impl = { graph_destroy };

    switch (interface) {
    case fa_equal_i:
        return &graph_equal_impl;

    case fa_string_show_i:
        return &graph_show_impl;

    case fa_copy_i:
        return &graph_copy_impl;

    case fa_destroy_i:
        return &graph_destroy_impl;

    default:
        return NULL;
    }
}

