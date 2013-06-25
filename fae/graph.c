
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/graph.h>
#include <fae/set.h>
#include <fae/string.h>
#include <fae/dynamic.h>
#include <fae/util.h>

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

struct _fae_graph_t {
    impl_t          impl;       //  Interface dispatcher
    set_t           nodes;      //  Set n
    map_t           edges;      //  Map (n,n) e
};


fae_ptr_t graph_impl(fae_id_t interface);
fae_ptr_t edge_impl(fae_id_t interface);

edge_t new_edge(ptr_t node1, ptr_t node2)
{
    edge_t edge   = fae_new_struct(edge);
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
    fae_delete(edge);
}

inline static graph_t new_graph(set_t nodes, map_t edges)
{
    graph_t graph     = fae_new(graph);
    graph->impl       = &graph_impl;
    graph->nodes      = nodes;
    graph->edges      = edges;
    return graph;
}

inline static void
delete_graph(graph_t graph)
{
    fae_delete(graph);
}


// --------------------------------------------------------------------------------

/** Create an empty graph.
 */
fae_graph_t fae_graph_empty()
{
    return new_graph(fae_set_empty(), fae_map_empty());
}

/** Insert a node in the given graph.
    @param node     Node to insert.
    @param graph    The graph.
 */
fae_graph_t fae_graph_insert(fae_graph_node_t node,
                             fae_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    return new_graph(fae_set_set(node, nodes), fae_copy(edges));
}

/** Remove a node from the given graph.
    @param node     Node to remove (comparing on equality).
    @param graph    The graph.
 */
fae_graph_t fae_graph_remove(fae_graph_node_t node,
                             fae_graph_t graph)
{
    assert(false && "Not implemented");
    // set_t nodes = graph->nodes;
    // map_t edges = graph->edges;
    // filter out entries not in node set
    // return new_graph(fae_set_remove(node, nodes), fae_copy(edges));
}

/** Copy the given graph.
    @param graph    The graph.
 */
fae_graph_t fae_graph_copy(fae_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    return new_graph(fae_copy(nodes), fae_copy(edges));
}

/** Destroy the given graph.
 */
void fae_graph_destroy(fae_graph_t graph)
{
    fae_destroy(graph->nodes);
    fae_destroy(graph->edges);
    delete_graph(graph);
}

/** Connect the given nodes.
    @param node1    First node in connection.
    @param node2    Second node in connection.
    @param label    Label to attach to connection.
    @param graph    The graph.
 */
fae_graph_t fae_graph_connect(fae_graph_node_t node1,
                              fae_graph_node_t node2,
                              fae_graph_edge_t label,
                              fae_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    edge_t edge = new_edge(node1, node2);

    if (fae_set_has(node1, nodes) && fae_set_has(node2, nodes)) {
        return new_graph(fae_copy(nodes), fae_map_set(edge, label, edges));
    } else {
        return fae_graph_copy(graph);
    }
}

/** Remove all connections between the given nodes.
    @param node1    First node in connection.
    @param node2    Second node in connection.
    @param graph    The graph.
 */
fae_graph_t fae_graph_disconnect(fae_graph_node_t node1,
                                 fae_graph_node_t node2,
                                 fae_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    edge_t edge = new_edge(node1, node2);
    return new_graph(fae_copy(nodes), fae_map_remove(edge, edges));
}


// --------------------------------------------------------------------------------

/** Convert the given graph to a [Dot](http://en.wikipedia.org/wiki/DOT_language)
    declaration.

    @param header           Text to include before graph declaration.
    @param inner_header     Text to include at the top of the graph declaration.
    @param graph            Graph to convert. Its node and edge values will be converted to
                            strings using [show](@ref fae_string_show). It the nodes
                            are pairs, their first component will be used as the node identifier
                            and the second as a label.

 */
fae_string_t fae_graph_to_dot(
    fae_string_t header,
    fae_string_t inner_header,
    fae_graph_t graph)
{
    string_t str = string("\n\n");
    char buf[100];

    str = string_dappend(str, header);
    str = string_dappend(str, string("\n\n"));

    str = string_dappend(str, string("digraph {\n"));

    str = string_dappend(str, inner_header);
    str = string_dappend(str, string("\n\n"));

    fae_set_for_each(node, graph->nodes) {
        if (fae_dynamic_get_type(node) == pair_type_repr) {
            char *cs = unstring(fae_string_to_string(fae_pair_first(node)));
            char *ds = unstring(fae_string_to_string(fae_pair_second(node)));

            snprintf(buf, 100, "    \"%s\" [label = \"%s\"];\n", cs, ds);
            str = string_dappend(str, string(buf));
        } else {
            char *cs = unstring(fae_string_to_string(node));

            snprintf(buf, 100, "    \"%s\";\n", cs);
            str = string_dappend(str, string(buf));
        }
    }
    fae_map_for_each(node_edge_pair, graph->edges) {
        ptr_t node1, node2, label;

        node1 = ((edge_t) fae_pair_first(node_edge_pair))->node1;
        node2 = ((edge_t) fae_pair_first(node_edge_pair))->node2;
        label = fae_pair_second(node_edge_pair);

        if (fae_dynamic_get_type(node1) == pair_type_repr) {
            node1   = fae_pair_first(node1);
            node2   = fae_pair_first(node2);
        }

        char *n1    = unstring(fae_string_to_string(node1));
        char *n2    = unstring(fae_string_to_string(node2));
        char *l     = unstring(fae_string_to_string(label));

        snprintf(buf, 100, "    \"%s\" -> \"%s\" [label=\"%s\"];\n", n1, n2, l);
        str = string_dappend(str, string(buf));
    }
    str = string_dappend(str, string("}\n\n"));
    return str;
}

// --------------------------------------------------------------------------------

bool edge_equal(fae_ptr_t a, fae_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return fae_equal(c->node1, d->node1)
           && fae_equal(c->node2, d->node2);
}

bool edge_less_than(fae_ptr_t a, fae_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return fae_less_than(c->node1, d->node1)
           || (fae_equal(c->node1, d->node1)
               && fae_less_than(c->node2, d->node2));
}

bool edge_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return fae_greater_than(c->node1, d->node1)
           || (fae_equal(c->node1, d->node1)
               && fae_greater_than(c->node2, d->node2));
}

fae_string_t edge_show(fae_ptr_t a)
{
    edge_t b = (edge_t) a;
    string_t s = string("(");
    s = string_dappend(s, fae_string_show(b->node1));
    s = string_dappend(s, string(","));
    s = string_dappend(s, fae_string_show(b->node2));
    s = string_dappend(s, string(")"));
    return s;
}

fae_ptr_t edge_copy(fae_ptr_t a)
{
    return copy_edge(a);
}

void edge_destroy(fae_ptr_t a)
{
    delete_edge(a);
}

fae_ptr_t edge_impl(fae_id_t interface)
{
    static fae_equal_t edge_equal_impl = { edge_equal };
    static fae_order_t edge_order_impl = { edge_less_than, edge_greater_than };
    static fae_string_show_t edge_show_impl = { edge_show };
    static fae_copy_t edge_copy_impl = { edge_copy };
    static fae_destroy_t edge_destroy_impl = { edge_destroy };

    switch (interface) {
    case fae_equal_i:
        return &edge_equal_impl;

    case fae_order_i:
        return &edge_order_impl;

    case fae_string_show_i:
        return &edge_show_impl;

    case fae_copy_i:
        return &edge_copy_impl;

    case fae_destroy_i:
        return &edge_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool graph_equal(fae_ptr_t a, fae_ptr_t b)
{
    graph_t c = (graph_t) a;
    graph_t d = (graph_t) b;
    return fae_equal(c->nodes, d->nodes)
           && fae_equal(c->edges, d->edges);
}

fae_string_t graph_show(fae_ptr_t x)
{
    graph_t graph = (graph_t) x;
    return fae_string_show(pair(graph->nodes, graph->edges)); // TODO dshow
}

fae_ptr_t graph_copy(fae_ptr_t a)
{
    return fae_graph_copy(a);
}

void graph_destroy(fae_ptr_t a)
{
    fae_graph_destroy(a);
}

fae_ptr_t graph_impl(fae_id_t interface)
{
    static fae_equal_t graph_equal_impl = { graph_equal };
    static fae_string_show_t graph_show_impl = { graph_show };
    static fae_copy_t graph_copy_impl = { graph_copy };
    static fae_destroy_t graph_destroy_impl = { graph_destroy };

    switch (interface) {
    case fae_equal_i:
        return &graph_equal_impl;

    case fae_string_show_i:
        return &graph_show_impl;

    case fae_copy_i:
        return &graph_copy_impl;

    case fae_destroy_i:
        return &graph_destroy_impl;

    default:
        return NULL;
    }
}

