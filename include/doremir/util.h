
#ifndef _DOREMIR_UTIL
#define _DOREMIR_UTIL

#include <doremir/util/alignof.h>
#include <doremir/util/minmax.h>
#include <doremir/util/literals.h>
#include <doremir/util/macros.h>
#include <doremir/util/apply.h>

#include <time.h>

#define system_time_t       time_t

#define ptr_t               doremir_ptr_t
#define impl_t              doremir_impl_t
#define pred_t              doremir_pred_t
#define nullary_t           doremir_nullary_t
#define unary_t             doremir_unary_t
#define binary_t            doremir_binary_t
#define ternary_t           doremir_ternary_t
#define error_t             doremir_error_t
#define error_callback_t    doremir_error_callback_t
#define severity_t          doremir_error_severity_t

#define pair_t              doremir_pair_t
#define list_t              doremir_list_t
#define set_t               doremir_set_t
#define map_t               doremir_map_t
#define graph_t             doremir_graph_t
#define string_t            doremir_string_t
#define type_repr_t         doremir_dynamic_type_repr_t
#define priority_queue_t    doremir_priority_queue_t
#define scheduler_t         doremir_scheduler_t
#define atomic_t            doremir_atomic_t
#define atomic_queue_t      doremir_atomic_queue_t
#define atomic_stack_t      doremir_atomic_stack_t
#define ringbuffer_t        doremir_atomic_ring_buffer_t
#ifndef NO_THREAD_T
#define thread_t            doremir_thread_t
#endif // NO_THREAD_T
#define mutex_t             doremir_thread_mutex_t

#define ratio_t             doremir_ratio_t
#define buffer_t            doremir_buffer_t
#define frames_t            doremir_type_frames_t
#define type_t              doremir_type_t
#define midi_t              doremir_midi_t
#define time_t              doremir_time_t
#define clock_t             doremir_time_clock_t
#define event_t             doremir_event_t
#define processor_t         doremir_processor_t
#define proc_t              doremir_processor_t
#define proc_interface_t    doremir_processor_interface_t
#define dispatcher_t        doremir_message_dispatcher_t
#define address_t           doremir_message_address_t
#define message_t           doremir_message_t

#define audio_device_t      doremir_device_audio_t
#define midi_device_t       doremir_device_midi_t
#define file_device_t       doremir_device_file_t
#define buffer_device_t     doremir_device_buffer_t
#define audio_stream_t      doremir_device_audio_stream_t
#define midi_stream_t       doremir_device_midi_stream_t
#define audio_session_t     doremir_device_audio_session_t
#define midi_session_t      doremir_device_midi_session_t
#define file_result_t       doremir_device_file_result_t
#define buffer_result_t     doremir_device_buffer_result_t

#define pair(a,b)           doremir_pair_create(a,b)
#define string(a)           doremir_string_from_utf8(a)
#define string16(a)         doremir_string_from_utf16(u##a)
#define string32(a)         doremir_string_from_utf32(U##a)
#define unstring(a)         doremir_string_to_utf8(a)
#define ratio(a,b)          doremir_ratio_create(a,b)
#define priority_queue()    doremir_priority_queue_empty()
#define atomic()            doremir_atomic_create()
#define atomic_queue()      doremir_atomic_queue_create()
#define atomic_stack()      doremir_atomic_stack_create()
#define atomic_ring_buffer(s) doremir_atomic_ring_buffer_create(s)
#define buffer(s)           doremir_buffer_create(s)
#define midi(s,a,b)         doremir_midi_create_simple(s,a,b)
#define midi_sysex(b)       doremir_midi_create_sysex(b)
#define dispatcher()        doremir_message_create_dispatcher()
#define lockfree_dispatcher() doremir_message_create_lockfree_dispatcher()

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

#define never()             doremir_event_never()
#define now(x)              doremir_event_now(x)
#define later(t,x)          doremir_event_later(t,x)
#define delay_event(t,x)    doremir_event_delay(t,x)
#define merge_event(x,y)    doremir_event_merge(x,y)
#define switch_event(p,x,y) doremir_event_switch(p,x,y)
#define receive_event(s,a)  doremir_event_receive(s,a)
#define send_event(s,a)     doremir_event_send(s,a,x)

#define unary(a,b,f,x)      doremir_processor_unary(a,b,f,x)
#define binary(a,b,c,f,x)   doremir_processor_binary(a,b,c,f,x)
#define split(a)            doremir_processor_split(a)
#define id(a)               doremir_processor_identity(a)
#define const(a,b)          doremir_processor_constant(a,b)
#define par(x,y)            doremir_processor_parallel(x,y)
#define comp(x,y)           doremir_processor_compose(x,y)
#define seq(x,y)            doremir_processor_sequence(x,y)
#define loop(x)             doremir_processor_loop(x)

#define empty               doremir_list_empty
#define cons                doremir_list_cons
#define is_empty            doremir_list_is_empty
#define char_at             doremir_string_char_at
#define string_append       doremir_string_append
#define string_dappend      doremir_string_dappend
#define format_integer      doremir_string_format_integer
#define format_floating     doremir_string_format_floating

void doremir_audio_engine_log_info(doremir_string_t);
void doremir_audio_engine_log_warning(doremir_string_t);
void doremir_audio_engine_log_error(doremir_string_t);

#define inform(s)           doremir_audio_engine_log_info(s)
#define warn(s)             doremir_audio_engine_log_warning(s)
#define fail(s)             doremir_audio_engine_log_error(s)
#define log_error(e)        doremir_error_log(NULL,e)

#define tb                  doremir_to_bool
#define ti8                 doremir_to_int8
#define ti16                doremir_to_int16
#define ti32                doremir_to_int32
#define ti64                doremir_to_int64
#define tf32                doremir_to_double
#define tf64                doremir_to_double
#define fb                  doremir_from_bool
// #define b                   doremir_from_bool
#define i8                  doremir_from_int8
#define i16                 doremir_from_int16
#define i32                 doremir_from_int32
#define i64                 doremir_from_int64
#define f32                 doremir_from_double
#define f64                 doremir_from_double

#endif // _DOREMIR_UTIL

