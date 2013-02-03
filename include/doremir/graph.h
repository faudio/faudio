
#ifndef _DOREMIR_GRAPH
#define _DOREMIR_GRAPH

#include <doremir.h>
#include <doremir/pair.h>
#include <doremir/list.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirGraph Graph
    @{
    */

typedef struct _doremir_graph_t * doremir_graph_t;
typedef doremir_ptr_t doremir_graph_node_t;
typedef doremir_ptr_t doremir_graph_edge_t;
typedef doremir_list_t doremir_graph_node_list_t;
typedef doremir_list_t doremir_graph_edge_list_t;
doremir_graph_t doremir_graph_empty();
doremir_graph_t doremir_graph_insert(doremir_graph_node_t,
                                     doremir_graph_t);
doremir_graph_t doremir_graph_remove(doremir_graph_node_t,
                                     doremir_graph_t);
void doremir_graph_destroy(doremir_graph_t);
doremir_graph_t doremir_graph_connect(doremir_graph_node_t,
                                      doremir_graph_node_t,
                                      doremir_graph_edge_t,
                                      doremir_graph_t);
doremir_graph_t doremir_graph_disconnect(doremir_graph_node_t,
                                         doremir_graph_node_t,
                                         doremir_graph_t);
doremir_string_t doremir_graph_to_dot(doremir_string_t,
                                      doremir_string_t,
                                      doremir_graph_t);
doremir_graph_t doremir_graph_from_list(doremir_list_t);
doremir_list_t doremir_graph_to_list(doremir_graph_t);

/** @}
    @}
    */

#endif // _DOREMIR_GRAPH

