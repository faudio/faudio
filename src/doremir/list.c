
#include <doremir/list.h>
#include <doremir/util.h>
#include <doremir/string.h>

/*  List entry, end represented by null. */
struct _node_t {
        size_t          count;
        struct _node_t* next;
        doremir_ptr_t   value;
    };  
typedef struct _node_t *node_t;

struct _doremir_list_t {
        doremir_impl_t  impl;
        node_t          node;
    };

inline static node_t
new_node(doremir_ptr_t x, node_t xs)
{
    node_t node = doremir_new_struct(node);
    node->count = 1;
    node->value = x;
    node->next  = xs;
    return node;
}

inline static node_t 
take_node(node_t node)
{
    if (node)
        node->count++; // TODO make atomic?
    return node;
}

inline static void
release_node (node_t node)
{
    if (!node) return;
    
    node->count--;
    if (node->count == 0)
    {
        release_node(node->next);
        doremir_delete(node);
    }
}             

doremir_ptr_t 
list_impl(doremir_id_t interface);

inline static list_t 
new_list(node_t node)
{
    list_t xs = doremir_new(list);
    xs->impl = &list_impl;    
    xs->node = node;
    return xs;
}

inline static void 
delete_list(list_t list)
{                   
    doremir_delete(list);
}

// --------------------------------------------------------------------------------

list_t doremir_list_empty()
{   
    list_t xs = new_list(NULL);
    return xs;
}

list_t doremir_list_single(doremir_ptr_t x)
{
    list_t xs = new_list(new_node(x, NULL));
    return xs;
}

list_t doremir_list_cons(doremir_ptr_t x, list_t xs)
{
    list_t ys = new_list(new_node(x, take_node(xs->node)));    
    return ys;
}

list_t doremir_list_consd(doremir_ptr_t x, list_t xs)
{
    list_t ys = new_list(new_node(x, xs->node));
    delete_list(xs);
    return ys;
}

list_t doremir_list_copy(list_t xs)
{
    list_t ys = new_list(take_node(xs->node));
    return ys;
}

void doremir_list_destroy(list_t xs)
{                      
    // TODO optionally map registered deleter (a la CoreFoundation?)
    release_node(xs->node);
    delete_list(xs);
}


// --------------------------------------------------------------------------------

list_t doremir_list(int count, ...)
{
    node_t n, *p;
    va_list ap;

    va_start(ap, count);
    n = NULL;
    p = &n;
    for (int i = 0; i < count; ++i)
    {
        doremir_ptr_t x = va_arg(ap,doremir_ptr_t);
        *p = new_node(x,NULL);
        p = &(*p)->next;
    }
    va_end(ap);                    

    return new_list(n);
} 

bool doremir_list_is_empty(list_t xs)
{
    return !xs->node;
}

int doremir_list_length(list_t xs)
{                                      
    int          count = 0;
    node_t node  = xs->node;
    while (node)
    {                     
        count++;
        node = node->next;
    }
    return count;
}

doremir_ptr_t doremir_list_head(list_t xs)
{   
    if (!xs->node) return NULL;
    return xs->node->value;
}

doremir_ptr_t list_tail(list_t xs)
{
    if (!xs->node) return NULL;
    return xs->node->next;
}

// --------------------------------------------------------------------------------

doremir_ptr_t doremir_list_init(list_t xs)
{
    assert(false && "Not implemented");
}

doremir_ptr_t doremir_list_last(list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_snoc(doremir_ptr_t x, list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_append(list_t xs, list_t ys)
{
    assert(false && "Not implemented");
}

list_t list_take(int n, list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_drop(int n, list_t xs)
{
    assert(false && "Not implemented");
}

bool doremir_list_is_elem(doremir_ptr_t x, list_t xs)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

list_t doremir_list_reverse(list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_sort(doremir_list_ord_list_t xs)
{
    assert(false && "Not implemented");
}

doremir_ptr_t doremir_list_find(doremir_pred_t p, list_t xs)
{
    assert(false && "Not implemented");
}

list_t doremir_list_filter(doremir_pred_t p, list_t xs)
{
    assert(false && "Not implemented");
}

// bool doremir_list_any(doremir_pred_t p, list_t xs)
// {
//     assert(false && "Not implemented");
// }
// 
// bool doremir_list_all(doremir_pred_t p, list_t xs)
// {
//     assert(false && "Not implemented");
// }

// --------------------------------------------------------------------------------

list_t doremir_list_map(doremir_unary_t f, list_t xs)
{
    node_t xn = xs->node;
    node_t yn = new_node(0, NULL);
    do
    {                        
        yn->value = xn->value;
        yn->next  = new_node(0, NULL);
        yn = yn->next;
        xn = xn->next;
    } while(xn);
    return new_list(yn);
}

// TODO this is foldl
// other variants?
doremir_ptr_t doremir_list_fold(doremir_binary_t f,
                                doremir_ptr_t    z,
                                list_t   xs)
{
    node_t xn = xs->node;
    doremir_ptr_t v = z;
    do
    {
        v  = f(v, xn->value);
        xn = xn->next;
    } while(xn);
    return v;
}

list_t doremir_list_concat(doremir_list_list_list_t xss)
{
    assert(false && "Not implemented");
}


// TODO type of z
doremir_ptr_t doremir_list_sum(list_t xs)
{
    return doremir_list_fold(doremir_add, fint32(0), xs);
}

doremir_ptr_t doremir_list_product(list_t xs)
{
    return doremir_list_fold(doremir_multiply, fint32(1), xs);
}

doremir_ptr_t doremir_list_maximum(list_t xs)
{
    return doremir_list_fold(doremir_max, fint32(INT_MIN), xs);
}

doremir_ptr_t doremir_list_minimum(list_t xs)
{
    return doremir_list_fold(doremir_min, fint32(INT_MAX), xs);
}

// --------------------------------------------------------------------------------

// list_t doremir_list_set_headd(doremir_ptr_t x, list_t xs)
// {                                 
//     assert(xs->node->count == 1);      
//     xs->node->value = x;
//     list_t ys = new_list(xs->node);
//     delete_list(xs);
//     return ys;
// }

// list_t doremir_list_snocd(doremir_ptr_t x, list_t xs)
// {
//     assert(false && "Not implemented");
// }
// 
// list_t doremir_list_reversed(list_t xs)
// {
//     assert(false && "Not implemented");
// }
// 
// list_t doremir_list_sortd(list_t xs)
// {
//     assert(false && "Not implemented");
// }
// 
// list_t doremir_list_mapd(doremir_unary_t f, list_t xs)
// {
//     assert(false && "Not implemented");
// }
// 
// list_t doremir_list_foldd(doremir_binary_t f,
//                                   doremir_ptr_t    z,
//                                   list_t   xs)
// {
//     assert(false && "Not implemented");
// }
// 
// --------------------------------------------------------------------------------


bool list_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    node_t an = ((list_t) a)->node;
    node_t bn = ((list_t) b)->node;
    while (an && bn)
    {
        if (doremir_not_equal(an->value, bn->value))
            return false;
        an = an->next;
        bn = bn->next;
    }                        
    return !(an || bn);
}

bool list_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    assert(false && "Not implemented");
}

bool list_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    assert(false && "Not implemented");
}

doremir_string_t list_show(doremir_ptr_t xs)
{                               
    node_t xn = ((list_t) xs)->node;
    string_t s = string("[");
    while(xn)
    {
        s = doremir_string_dappend(s, doremir_string_show(xn->value));
        xn = xn->next;
        if (xn) 
            s = doremir_string_dappend(s, string(","));
    };
    s = sdappend(s, string("]"));
    return s;
}

doremir_ptr_t list_copy(doremir_ptr_t a)
{
    return doremir_list_copy(a);
}

void list_destroy(doremir_ptr_t a)
{
    doremir_list_destroy(a);
}

doremir_ptr_t list_impl(doremir_id_t interface)
{
    static doremir_equal_t list_equal_impl = { list_equal };
    static doremir_copy_t list_copy_impl = { list_copy };
    static doremir_string_show_t list_show_impl = { list_show };
    static doremir_destroy_t list_destroy_impl = { list_destroy };
    static doremir_order_t list_order_impl = { list_less_than, list_greater_than };

    switch (interface)
    {
    case doremir_equal_i:
        return &list_equal_impl;

    case doremir_order_i:
        return &list_order_impl;

    case doremir_string_show_i:
        return &list_show_impl;

    case doremir_copy_i:
        return &list_copy_impl;

    case doremir_destroy_i:
        return &list_destroy_impl;

    default:
        return NULL;
    }
}
