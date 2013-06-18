
#ifndef _FAE_UTIL
#define _FAE_UTIL

#include <fae/util/arch.h>
#include <fae/util/alignof.h>
#include <fae/util/minmax.h>
#include <fae/util/literals.h>
#include <fae/util/macros.h>
#include <fae/util/apply.h>

#include <time.h>

#define system_time_t       time_t

#define ptr_t               fae_ptr_t
#define impl_t              fae_impl_t
#define pred_t              fae_pred_t
#define nullary_t           fae_nullary_t
#define unary_t             fae_unary_t
#define binary_t            fae_binary_t
#define ternary_t           fae_ternary_t
#define error_t             fae_error_t
#define error_callback_t    fae_error_callback_t
#define severity_t          fae_error_severity_t

#define pair_t              fae_pair_t
#define list_t              fae_list_t
#define set_t               fae_set_t
#define map_t               fae_map_t
#define graph_t             fae_graph_t
#define string_t            fae_string_t
#define type_repr_t         fae_dynamic_type_repr_t
#define priority_queue_t    fae_priority_queue_t
#define scheduler_t         fae_scheduler_t
#define atomic_t            fae_atomic_t
#define atomic_queue_t      fae_atomic_queue_t
#define atomic_stack_t      fae_atomic_stack_t
#define ringbuffer_t        fae_atomic_ring_buffer_t
#ifndef NO_THREAD_T
#define thread_t            fae_thread_t
#endif // NO_THREAD_T
#define mutex_t             fae_thread_mutex_t

#define ratio_t             fae_ratio_t
#define buffer_t            fae_buffer_t
#define frames_t            fae_type_frames_t
#define type_t              fae_type_t
#define midi_msg_t          fae_midi_msg_t
#define time_t              fae_time_t
#define clock_t             fae_time_clock_t
#define event_t             fae_event_t
#define processor_t         fae_processor_t
#define proc_t              fae_processor_t
#define proc_interface_t    fae_processor_interface_t
#define sender_t            fae_message_sender_t
#define receiver_t          fae_message_receiver_t
#define dispatcher_t        fae_message_dispatcher_t
#define address_t           fae_message_address_t
#define message_t           fae_message_t

#define audio_device_t      fae_audio_device_t
#define midi_device_t       fae_midi_device_t
#define file_device_t       fae_device_file_t
#define buffer_device_t     fae_device_buffer_t
#define audio_stream_t      fae_audio_stream_t
#define midi_stream_t       fae_midi_stream_t
#define audio_session_t     fae_audio_session_t
#define midi_session_t      fae_midi_session_t
#define file_result_t       fae_device_file_result_t
#define buffer_result_t     fae_device_buffer_result_t

#define pair(a,b)           fae_pair_create(a,b)
#define string(a)           fae_string_from_utf8(a)
#define string16(a)         fae_string_from_utf16(u##a)
#define string32(a)         fae_string_from_utf32(U##a)
#define unstring(a)         fae_string_to_utf8(a)
#define ratio(a,b)          fae_ratio_create(a,b)
#define priority_queue()    fae_priority_queue_empty()
#define atomic()            fae_atomic_create()
#define atomic_queue()      fae_atomic_queue_create()
#define atomic_stack()      fae_atomic_stack_create()
#define atomic_ring_buffer(s) fae_atomic_ring_buffer_create(s)
#define buffer(s)           fae_buffer_create(s)
#define midi_msg(s,a,b)     fae_midi_msg_create_simple(s,a,b)
#define midi_msg_sysex(b)   fae_midi_msg_create_sysex(b)
#define dispatcher()        fae_message_create_dispatcher()
#define lockfree_dispatcher() fae_message_create_lockfree_dispatcher()

#define type(a)             fae_type_simple(a##_type)
#define type_pair(a,b)      fae_type_pair(a,b)
#define type_vector(a,n)    fae_type_vector(a,n)
#define type_frame(a)       fae_type_frame(a)

#define hms(h,m,s)          fae_time_create(0,h,m,ratio(s,1))
#define days(d)             fae_time_create(d,0,0,ratio(0,1))
#define hours(h)            fae_time_create(0,h,0,ratio(0,1))
#define minutes(m)          fae_time_create(0,0,m,ratio(0,1))
#define seconds(s)          fae_time_create(0,0,0,ratio(s,1))
#define divisions(a,b)      fae_time_create(0,0,0,ratio(a,b))
#define milliseconds(s)     fae_time_create(0,0,0,ratio(s,1000))
#define microseconds(s)     fae_time_create(0,0,0,ratio(s,1000000))

#define never()             fae_event_never()
#define now(x)              fae_event_now(x)
#define later(t,x)          fae_event_later(t,x)
#define delay_event(t,x)    fae_event_delay(t,x)
#define merge_event(x,y)    fae_event_merge(x,y)
#define switch_event(p,x,y) fae_event_switch(p,x,y)
#define receive_event(s,a)  fae_event_receive(s,a)
#define send_event(s,a)     fae_event_send(s,a,x)

#define before_event(p,x)   fae_event_before(p,x)
#define after_event(p,x)    fae_event_after(p,x)
#define head_event(x)       fae_event_head(x)
#define tail_event(x)       fae_event_tail(fae_event_offset(x),x)

#define unary(a,b,f,x)      fae_processor_unary(a,b,f,x)
#define binary(a,b,c,f,x)   fae_processor_binary(a,b,c,f,x)
#define split(a)            fae_processor_split(a)
#define id(a)               fae_processor_identity(a)
#define const(a,b)          fae_processor_constant(a,b)
#define par(x,y)            fae_processor_parallel(x,y)
#define comp(x,y)           fae_processor_compose(x,y)
#define seq(x,y)            fae_processor_sequence(x,y)
#define loop(x)             fae_processor_loop(x)

#define empty               fae_list_empty
#define cons                fae_list_cons
#define is_empty            fae_list_is_empty
#define char_at             fae_string_char_at
#define string_append       fae_string_append
#define string_dappend      fae_string_dappend
#define format_integral     fae_string_format_integral
#define format_floating     fae_string_format_floating

void fae_fae_log_info(fae_string_t);
void fae_fae_log_warning(fae_string_t);
void fae_fae_log_error(fae_string_t);
void fae_fae_dlog_info(fae_string_t);

#define inform(s)           fae_fae_log_info(s)
#define dinform(s)          fae_fae_dlog_info(s)
#define warn(s)             fae_fae_log_warning(s)
#define fail(s)             fae_fae_log_error(s)
#define log_error(e)        fae_error_log(NULL,e)

#define tb                  fae_to_bool
#define ti8                 fae_to_int8
#define ti16                fae_to_int16
#define ti32                fae_to_int32
#define ti64                fae_to_int64
#define tf32                fae_to_double
#define tf64                fae_to_double
#define fb                  fae_from_bool
// #define b                   fae_from_bool
#define i8                  fae_from_int8
#define i16                 fae_from_int16
#define i32                 fae_from_int32
#define i64                 fae_from_int64
#define f32                 fae_from_double
#define f64                 fae_from_double

#endif // _FAE_UTIL

