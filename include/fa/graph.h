
#ifndef _FA_GRAPH
#define _FA_GRAPH

#include <fa.h>
#include <fa/pair.h>
#include <fa/list.h>
#include <fa/string.h>

/** @addtogroup FaGraph
 
    @addtogroup FaGraph
    
    Immutable directed graph data structure.
    
    @warning
        Not fully implemented.

    @par Literals

    @par Requires 
    - fa_equal_t

    @par Implements 
    - fa_equal_t
    - fa_copy_t
    - fa_destroy_t
    - fa_string_show_t

    @see 
    - [Data structures](@ref DataStructures)
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaGraph Graph
    @{
    */


typedef struct _fa_graph_t * fa_graph_t;


typedef fa_ptr_t fa_graph_node_t;


typedef fa_ptr_t fa_graph_edge_t;


typedef fa_list_t fa_graph_node_list_t;


typedef fa_list_t fa_graph_edge_list_t;


fa_graph_t fa_graph_empty();


fa_graph_t fa_graph_insert(fa_graph_node_t, fa_graph_t);


fa_graph_t fa_graph_remove(fa_graph_node_t, fa_graph_t);


void fa_graph_destroy(fa_graph_t);


fa_graph_t fa_graph_connect(fa_graph_node_t,
                            fa_graph_node_t,
                            fa_graph_edge_t,
                            fa_graph_t);


fa_string_t fa_graph_to_dot(fa_string_t, fa_string_t, fa_graph_t);

/** @}
    @}
    */

#endif // _FA_GRAPH

