
#ifndef _DOREMIR_UTIL
#define _DOREMIR_UTIL

#include <doremir/util/alignof.h>
#include <doremir/util/literals.h>
#include <doremir/util/macros.h>

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

#define hms(h,m,s)          doremir_time_create(0,h,m,ratio(s,1))
#define days(d)             doremir_time_create(d,0,0,ratio(0,1))
#define hours(h)            doremir_time_create(0,h,0,ratio(0,1))
#define minutes(m)          doremir_time_create(0,0,m,ratio(0,1))
#define seconds(s)          doremir_time_create(0,0,0,ratio(s,1))
#define divisions(a,b)      doremir_time_create(0,0,0,ratio(a,b))
#define milliseconds(s)     doremir_time_create(0,0,0,ratio(s,1000))
#define microseconds(s)     doremir_time_create(0,0,0,ratio(s,1000000))

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

#define empty               doremir_list_empty
#define cons                doremir_list_cons
#define snoc                doremir_list_snoc
#define append              doremir_list_append
#define is_empty            doremir_list_is_empty
#define char_at             doremir_string_char_at
#define string_append       doremir_string_append
#define string_dappend      doremir_string_dappend
#define format_int          doremir_string_format_integer
#define format_integer      doremir_string_format_integer

#define aexchange           doremir_atomic_exchange
#define aget                doremir_atomic_get
#define aset                doremir_atomic_set
#define aadd                doremir_atomic_add
#define amodify             doremir_atomic_modify

#endif // _DOREMIR_UTIL

