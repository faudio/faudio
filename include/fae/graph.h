
#ifndef _FAE_GRAPH
#define _FAE_GRAPH

#include <fae.h>
#include <fae/pair.h>
#include <fae/list.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeGraph Graph
    @{
    */

typedef struct _fae_graph_t * fae_graph_t;
typedef fae_ptr_t fae_graph_node_t;
typedef fae_ptr_t fae_graph_edge_t;
typedef fae_list_t fae_graph_node_list_t;
typedef fae_list_t fae_graph_edge_list_t;
fae_graph_t fae_graph_empty();
fae_graph_t fae_graph_insert(fae_graph_node_t, fae_graph_t);
fae_graph_t fae_graph_remove(fae_graph_node_t, fae_graph_t);
void fae_graph_destroy(fae_graph_t);
fae_graph_t fae_graph_connect(fae_graph_node_t,
                              fae_graph_node_t,
                              fae_graph_edge_t,
                              fae_graph_t);
fae_graph_t fae_graph_disconnect(fae_graph_node_t,
                                 fae_graph_node_t,
                                 fae_graph_t);
fae_string_t fae_graph_to_dot(fae_string_t,
                              fae_string_t,
                              fae_graph_t);
fae_graph_t fae_graph_from_list(fae_list_t);
fae_list_t fae_graph_to_list(fae_graph_t);

/** @}
    @}
    */

#endif // _FAE_GRAPH

