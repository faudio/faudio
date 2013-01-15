
#ifndef _DOREMIR_UTIL
#define _DOREMIR_UTIL

#define ptr_t               doremir_ptr_t
#define impl_t              doremir_impl_t
#define pred_t              doremir_pred_t
#define nullary_t           doremir_nullary_t
#define unary_t             doremir_unary_t
#define binary_t            doremir_binary_t
#define ternary_t           doremir_ternary_t
#define pair_t              doremir_pair_t
#define list_t              doremir_list_t
#define set_t               doremir_set_t
#define map_t               doremir_map_t
#define string_t            doremir_string_t
#define atomic_t            doremir_atomic_t
#define atomic_queue_t      doremir_atomic_queue_t
#define atomic_stack_t      doremir_atomic_stack_t
#define ringbuffer_t        doremir_atomic_ring_buffer_t
#define priority_queue_t    doremir_priority_queue_t
#define ratio_t             doremir_ratio_t
#define buffer_t            doremir_buffer_t
#define thread_t            doremir_thread_t
#define mutex_t             doremir_thread_mutex_t
#define processor_t         doremir_processor_t
#define dispatcher_t        doremir_message_dispatcher_t
#define time_t              doremir_time_t
#define type_t              doremir_type_t

#define atomic()            doremir_atomic_create()
#define pair(a,b)           doremir_pair_create(a,b)
#define range(a,b)          doremir_list_enumerate(a,b)
#define string(a)           doremir_string_from_utf8(a)
#define unstring(a)         doremir_string_to_utf8(a)
#define ratio(a,b)          doremir_ratio_create(a,b)

#define type(a)             doremir_type_simple(a##_type)
#define type_pair(a,b)      doremir_type_pair(a,b)
#define type_vector(a,n)    doremir_type_vector(a,n)
#define type_frame(a)       doremir_type_frame(a)

#define time(d,h,m,s)       doremir_time_create(d,h,m,ratio(s,1))
#define days(d)             time(d,0,0,0)
#define hours(h)            time(0,h,0,0)
#define minutes(m)          time(0,0,m,0)
#define seconds(s)          time(0,0,0,s)
#define milliseconds(s)     doremir_time_create(0,0,0,ratio(s,1000))

#define tb                  doremir_to_bool
#define ti8                 doremir_to_int8
#define ti16                doremir_to_int16
#define ti32                doremir_to_int32
#define ti64                doremir_to_int64
#define td                  doremir_to_double
#define b                   doremir_from_bool
#define i8                  doremir_from_int8
#define i16                 doremir_from_int16
#define i32                 doremir_from_int32
#define i64                 doremir_from_int64
#define d                   doremir_from_double

#define eq                  doremir_equal
#define gt                  doremir_greater_than
#define lt                  doremir_less_than
#define gte                 doremir_greater_than_equal
#define lte                 doremir_less_than_equal

#define empty               doremir_list_empty
#define cons                doremir_list_cons
#define snoc                doremir_list_snoc
#define append              doremir_list_append
#define destroy             doremir_list_destroy
#define copy                doremir_list_copy
#define swap                doremir_list_swap
#define is_empty            doremir_list_is_empty
#define is_single           doremir_list_is_single
#define lenght              doremir_list_lenght
#define head                doremir_list_head
#define tail                doremir_list_tail
#define init                doremir_list_init
#define last                doremir_list_last
#define take                doremir_list_take
#define drop                doremir_list_drop
#define has_elem            doremir_list_has_elem
#define reverse             doremir_list_reverse
#define sort                doremir_list_sort
#define filter              doremir_list_filter
#define consd               doremir_list_consd
#define snocd               doremir_list_snocd
#define reversed            doremir_list_reversed
#define sortd               doremir_list_sortd
#define mapd                doremir_list_mapd

#define slength             doremir_string_length
#define scopy               doremir_string_copy
#define sappend             doremir_string_append
#define sdappend            doremir_string_dappend
#define char_at             doremir_string_char_at
#define sshow               doremir_string_show

#define string_dappend      doremir_string_dappend
#define format_int          doremir_string_format_integer
#define format_integer      doremir_string_format_integer

#define aexchange           doremir_atomic_exchange
#define aget                doremir_atomic_get
#define aset                doremir_atomic_set
#define aadd                doremir_atomic_add
#define amodify             doremir_atomic_modify


// TODO should this be here...?

#ifndef alignof
#define alignof(T) offsetof (struct { char c; T member; }, member)
#endif



// TODO literals to be moved someplace else...

#include <doremir/list.h> // for list forward decl
#include <doremir/set.h> // for set forward decl
#include <doremir/map.h> // for map forward decl

list_t doremir_list(int count, ...);
set_t doremir_set(int count, ...);
map_t doremir_map(int count, ...);

#define VA_NARGS_IMPL(_1, _2, _3, _4, _5, _6, N, ...) N
#define VA_NARGS(...) VA_NARGS_IMPL(X,##__VA_ARGS__, 5, 4, 3, 2, 1, 0)
#define VARARG_IMPL2(base, count, ...) base##count(__VA_ARGS__)
#define VARARG_IMPL(base, count, ...) VARARG_IMPL2(base, count, __VA_ARGS__)
#define VARARG(base, ...) VARARG_IMPL(base, VA_NARGS(__VA_ARGS__), __VA_ARGS__)

#define list0()                             doremir_list(0)
#define list1(a)                            doremir_list(1,a)
#define list2(a,b)                          doremir_list(2,a,b)
#define list3(a,b,c)                        doremir_list(3,a,b,c)
#define list4(a,b,c,d)                      doremir_list(4,a,b,c,d)
#define list5(a,b,c,d,e)                    doremir_list(5,a,b,c,d,e)
#define list(...) VARARG(list, __VA_ARGS__)

#define set0()                              doremir_set(0)
#define set1(a)                             doremir_set(1,a)
#define set2(a,b)                           doremir_set(2,a,b)
#define set3(a,b,c)                         doremir_set(3,a,b,c)
#define set4(a,b,c,d)                       doremir_set(4,a,b,c,d)
#define set5(a,b,c,d,e)                     doremir_set(5,a,b,c,d,e)
#define set(...) VARARG(set, __VA_ARGS__)

#define map0()                              doremir_map(0)
#define map1(a)                             doremir_map(1,a)
#define map2(a,b)                           doremir_map(2,a,b)
#define map3(a,b,c)                         doremir_map(3,a,b,c)
#define map4(a,b,c,d)                       doremir_map(4,a,b,c,d)
#define map5(a,b,c,d,e)                     doremir_map(5,a,b,c,d,e)
#define map(...) VARARG(list, __VA_ARGS__)



// TODO should also be moved

#define doremir_let(type, binding) \
    for (type binding,*_c=((type*)1);_c;_c=((type*)0))

#define doremir_list_for_each(list, is_last, var) \
    for(list_t xs = list;                             \
        !doremir_list_is_empty(xs);                   \
        xs = doremir_list_tail(xs)                    \
        )                                             \
    doremir_let(ptr_t, var = doremir_list_head(xs))   \
        doremir_let(bool, is_last = doremir_list_is_single(xs))



#endif // _DOREMIR_UTIL

