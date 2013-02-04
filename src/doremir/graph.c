
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
        FIXME only allows one connection per node pair
 */

struct edge {
    impl_t      impl;       //  Interface dispatcher
    ptr_t       node1;
    ptr_t       node2;
};

typedef ptr_t         node_t;
typedef struct edge *edge_t;

struct _doremir_graph_t {
    impl_t          impl;       //  Interface dispatcher
    set_t           nodes;      //  Set n
    map_t           edges;      //  Map (n,n) e
};

doremir_ptr_t graph_impl(doremir_id_t interface);
doremir_ptr_t edge_impl(doremir_id_t interface);

edge_t new_edge(ptr_t node1, ptr_t node2)
{
    edge_t edge   = doremir_new_struct(edge);
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
    doremir_delete(edge);
}

inline static graph_t new_graph(set_t nodes, map_t edges)
{
    graph_t graph     = doremir_new(graph);
    graph->impl       = &graph_impl;
    graph->nodes      = nodes;
    graph->edges      = edges;
    return graph;
}

inline static void
delete_graph(graph_t graph)
{
    doremir_delete(graph);
}


// --------------------------------------------------------------------------------

/** Create an empty graph.
 */
doremir_graph_t doremir_graph_empty()
{
    return new_graph(doremir_set_empty(), doremir_map_empty());
}

/** Insert a node in the given graph.
    @param node     Node to insert.
    @param graph    The graph.
 */
doremir_graph_t doremir_graph_insert(doremir_graph_node_t node,
                                     doremir_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    return new_graph(doremir_set_set(node, nodes), doremir_copy(edges));
}

/** Remove a node from the given graph.
    @param node     Node to remove (comparing on equality).
    @param graph    The graph.
 */
doremir_graph_t doremir_graph_remove(doremir_graph_node_t node,
                                     doremir_graph_t graph)
{
    assert(false && "Not implemented");
    // set_t nodes = graph->nodes;
    // map_t edges = graph->edges;
    // filter out entries not in node set
    // return new_graph(doremir_set_remove(node, nodes), doremir_copy(edges));
}

/** Copy the given graph.
    @param graph    The graph.
 */
doremir_graph_t doremir_graph_copy(doremir_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    return new_graph(doremir_copy(nodes), doremir_copy(edges));
}

/** Destroy the given graph.
 */
void doremir_graph_destroy(doremir_graph_t graph)
{
    doremir_destroy(graph->nodes);
    doremir_destroy(graph->edges);
    delete_graph(graph);
}

/** Connect the given nodes.
    @param node1    First node in connection.
    @param node2    Second node in connection.
    @param label    Label to attach to connection.
    @param graph    The graph.
 */
doremir_graph_t doremir_graph_connect(doremir_graph_node_t node1,
                                      doremir_graph_node_t node2,
                                      doremir_graph_edge_t label,
                                      doremir_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    edge_t edge = new_edge(node1, node2);

    if (doremir_set_has(node1, nodes) && doremir_set_has(node2, nodes)) {
        return new_graph(doremir_copy(nodes), doremir_map_set(edge, label, edges));
    } else {
        return doremir_graph_copy(graph);
    }
}

/** Remove all connections between the given nodes.
    @param node1    First node in connection.
    @param node2    Second node in connection.
    @param graph    The graph.
 */
doremir_graph_t doremir_graph_disconnect(doremir_graph_node_t node1,
        doremir_graph_node_t node2,
        doremir_graph_t graph)
{
    set_t nodes = graph->nodes;
    map_t edges = graph->edges;
    edge_t edge = new_edge(node1, node2);
    return new_graph(doremir_copy(nodes), doremir_map_remove(edge, edges));
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

/** Convert the given graph to a [Dot](http://en.wikipedia.org/wiki/DOT_language) 
    declaration.
    
    @param header           Text to include before graph declaration.
    @param inner_header     Text to include at the top of the graph declaration.
    @param graph            Graph to convert. Its node and edge values will be converted to
                            strings using [show](@ref doremir_string_show). It the nodes
                            are pairs, their first component will be used as the node identifier
                            and the second as a label.
                            
 */
doremir_string_t doremir_graph_to_dot(
    doremir_string_t header,
    doremir_string_t inner_header,
    doremir_graph_t graph)
{
    string_t str = string("\n\n");
    char buf[100];

    str = string_dappend(str, header);
    str = string_dappend(str, string("\n\n"));

    str = string_dappend(str, string("digraph {\n"));

    str = string_dappend(str, inner_header);
    str = string_dappend(str, string("\n\n"));

    doremir_set_for_each(x, graph->nodes) {
        char *cs = doremir_string_to_utf8(doremir_string_show(x));
        snprintf(buf, 100, "    %s;\n", cs);
        str = string_dappend(str, string(buf));
    }
    doremir_map_for_each(x, graph->edges) {
        char *n1 = doremir_string_to_utf8(doremir_string_show(doremir_pair_fst(doremir_pair_fst(x))));
        char *n2 = doremir_string_to_utf8(doremir_string_show(doremir_pair_snd(doremir_pair_fst(x))));
        char *l = doremir_string_to_utf8(doremir_string_show(doremir_pair_snd(x)));  
        // FIXME want an unescaped show here

        snprintf(buf, 100, "    %s -> %s [label=%s];\n", n1, n2, l);
        str = string_dappend(str, string(buf));
    }
    str = string_dappend(str, string("}\n\n"));
    return str;
}

// --------------------------------------------------------------------------------

bool edge_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return doremir_equal(c->node1, d->node1)
           && doremir_equal(c->node2, d->node2);
}

bool edge_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return doremir_less_than(c->node1, d->node1)
           || (doremir_equal(c->node1, d->node1)
               && doremir_less_than(c->node2, d->node2));
}

bool edge_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    edge_t c = (edge_t) a;
    edge_t d = (edge_t) b;
    return doremir_greater_than(c->node1, d->node1)
           || (doremir_equal(c->node1, d->node1)
               && doremir_greater_than(c->node2, d->node2));
}

doremir_string_t edge_show(doremir_ptr_t a)
{
    edge_t b = (edge_t) a;
    string_t s = string("(");
    s = string_dappend(s, doremir_string_show(b->node1));
    s = string_dappend(s, string(","));
    s = string_dappend(s, doremir_string_show(b->node2));
    s = string_dappend(s, string(")"));
    return s;
}

doremir_ptr_t edge_copy(doremir_ptr_t a)
{
    return copy_edge(a);
}

void edge_destroy(doremir_ptr_t a)
{
    delete_edge(a);
}

doremir_ptr_t edge_impl(doremir_id_t interface)
{
    static doremir_equal_t edge_equal_impl = { edge_equal };
    static doremir_order_t edge_order_impl = { edge_less_than, edge_greater_than };
    static doremir_string_show_t edge_show_impl = { edge_show };
    static doremir_copy_t edge_copy_impl = { edge_copy };
    static doremir_destroy_t edge_destroy_impl = { edge_destroy };

    switch (interface) {
        case doremir_equal_i:
            return &edge_equal_impl;

        case doremir_order_i:
            return &edge_order_impl;

        case doremir_string_show_i:
            return &edge_show_impl;

        case doremir_copy_i:
            return &edge_copy_impl;

        case doremir_destroy_i:
            return &edge_destroy_impl;

        default:
            return NULL;
    }
}


// --------------------------------------------------------------------------------

bool graph_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    graph_t c = (graph_t) a;
    graph_t d = (graph_t) b;
    return doremir_equal(c->nodes, d->nodes)
           && doremir_equal(c->edges, d->edges);
}

doremir_string_t graph_show(doremir_ptr_t x)
{
    graph_t graph = (graph_t) x;
    return doremir_string_show(pair(graph->nodes, graph->edges)); // TODO dshow
}

doremir_ptr_t graph_copy(doremir_ptr_t a)
{
    return doremir_graph_copy(a);
}

void graph_destroy(doremir_ptr_t a)
{
    doremir_graph_destroy(a);
}

doremir_ptr_t graph_impl(doremir_id_t interface)
{
    static doremir_equal_t graph_equal_impl = { graph_equal };
    static doremir_string_show_t graph_show_impl = { graph_show };
    static doremir_copy_t graph_copy_impl = { graph_copy };
    static doremir_destroy_t graph_destroy_impl = { graph_destroy };

    switch (interface) {
        case doremir_equal_i:
            return &graph_equal_impl;

        case doremir_string_show_i:
            return &graph_show_impl;

        case doremir_copy_i:
            return &graph_copy_impl;

        case doremir_destroy_i:
            return &graph_destroy_impl;

        default:
            return NULL;
    }
}

