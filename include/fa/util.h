
#ifndef _FA_UTIL
#define _FA_UTIL

#include <fa/util/arch.h>
#include <fa/util/alignof.h>
#include <fa/util/minmax.h>
#include <fa/util/literals.h>
#include <fa/util/macros.h>
#include <fa/util/apply.h>

#include <time.h>

#define system_time_t       time_t

// #define ptr_t               fa_ptr_t
// #define impl_t              fa_impl_t
// #define pred_t              fa_pred_t
// #define nullary_t           fa_nullary_t
// #define unary_t             fa_unary_t
// #define binary_t            fa_binary_t
// #define ternary_t           fa_ternary_t
// #define error_t             fa_error_t
// #define error_callback_t    fa_error_callback_t
// #define severity_t          fa_error_severity_t

#define pair_t              fa_pair_t
#define pair_left_t         fa_pair_left_t
#define list_t              fa_list_t
#define set_t               fa_set_t
#define map_t               fa_map_t
#define graph_t             fa_graph_t
#define string_t            fa_string_t
#define type_repr_t         fa_dynamic_type_repr_t
#define priority_queue_t    fa_priority_queue_t
#define scheduler_t         fa_scheduler_t
#define atomic_t            fa_atomic_t
#define atomic_queue_t      fa_atomic_queue_t
#define atomic_stack_t      fa_atomic_stack_t
#define ringbuffer_t        fa_atomic_ring_buffer_t
#ifndef NO_THREAD_T
#define thread_t            fa_thread_t
#endif // NO_THREAD_T
#define mutex_t             fa_thread_mutex_t

#define ratio_t             fa_ratio_t
#define buffer_t            fa_buffer_t
#define frames_t            fa_type_frames_t
#define type_t              fa_type_t
#define midi_message_t      fa_midi_message_t
#define time_t              fa_time_t
#define clock_t             fa_time_clock_t
#define event_t             fa_event_t
#define signal_t            fa_signal_t
#define sender_t            fa_message_sender_t
#define receiver_t          fa_message_receiver_t
#define dispatcher_t        fa_message_dispatcher_t
#define address_t           fa_message_address_t
#define message_t           fa_message_t

#define audio_device_t      fa_audio_device_t
#define midi_device_t       fa_midi_device_t
#define file_device_t       fa_device_file_t
#define buffer_device_t     fa_device_buffer_t
#define audio_stream_t      fa_audio_stream_t
#define midi_stream_t       fa_midi_stream_t
#define audio_session_t     fa_audio_session_t
#define midi_session_t      fa_midi_session_t
#define file_result_t       fa_device_file_result_t
#define buffer_result_t     fa_device_buffer_result_t

// #define pair(a,b)           fa_pair_create(a,b)
// #define pair_left(a,b)      fa_pair_left_create(a,b)
// #define string(a)           fa_string_from_utf8(a)
#define fa_string(a)           fa_string_from_utf8(a)
// #define string16(a)         fa_string_from_utf16(u##a)
// #define string32(a)         fa_string_from_utf32(U##a)
// #define unstring(a)         fa_string_to_utf8(a)
#define fa_unstring(a)         fa_string_to_utf8(a)
#define fa_ratio(a,b)          fa_ratio_create(a,b)
#define fa_priority_queue()    fa_priority_queue_empty()
#define fa_atomic()            fa_atomic_create()
#define fa_atomic_queue()      fa_atomic_queue_create()
#define fa_atomic_stack()      fa_atomic_stack_create()
#define fa_atomic_ring_buffer(s) fa_atomic_ring_buffer_create(s)
#define fa_buffer(s)           fa_buffer_create(s)
#define fa_midi_message(s,a,b) fa_midi_message_create_simple(s,a,b)
#define fa_midi_message_sysex(b) fa_midi_message_create_sysex(b)

#define fa_action_set          fa_action_set
#define fa_action_accum        fa_action_accum
#define fa_action_send         fa_action_send

#define fa_hms(h,m,s)          fa_time_create(0,h,m,fa_ratio(s,1))
#define fa_days(d)             fa_time_create(d,0,0,fa_ratio(0,1))
#define fa_hours(h)            fa_time_create(0,h,0,fa_ratio(0,1))
#define fa_minutes(m)          fa_time_create(0,0,m,fa_ratio(0,1))
#define fa_seconds(s)          fa_time_create(0,0,0,fa_ratio(s,1))
#define fa_divisions(a,b)      fa_time_create(0,0,0,fa_ratio(a,b))
#define fa_milliseconds(s)     fa_time_create(0,0,0,fa_ratio(s,1000))
#define fa_microseconds(s)     fa_time_create(0,0,0,fa_ratio(s,1000000))

// #define stime               fa_signal_time
// #define srandom             fa_signal_random
#define fa_constant            fa_signal_constant
#define fa_impulse            fa_signal_impulse
#define fa_line                fa_signal_line
#define fa_delay               fa_signal_delay
#define fa_loop                fa_signal_loop

#define fa_empty               fa_list_empty
#define fa_cons                fa_list_cons
#define fa_is_empty            fa_list_is_empty

#define fa_char_at             fa_string_char_at
#define fa_string_append       fa_string_append
#define fa_string_dappend      fa_string_dappend
#define fa_format_integral     fa_string_format_integral
#define fa_format_floating     fa_string_format_floating

void fa_log_info(fa_string_t);
void fa_log_warning(fa_string_t);
void fa_log_error(fa_string_t);
void fa_dlog_info(fa_string_t);

#define fa_inform(s)           fa_log_info(s)
// #define fa_dinform(s)          fa_dlog_info(s)
#define fa_warn(s)             fa_log_warning(s)
#define fa_fail(s)             fa_log_error(s)
// #define fa_log_error(e)        fa_error_log(NULL,e)

#define tb                  fa_to_bool
#define ti8                 fa_to_int8
#define ti16                fa_to_int16
#define ti32                fa_to_int32
#define ti64                fa_to_int64
#define tf32                fa_to_double
#define tf64                fa_to_double
#define fb                  fa_from_bool
// #define b                   fa_from_bool
#define i8                  fa_from_int8
#define i16                 fa_from_int16
#define i32                 fa_from_int32
#define i64                 fa_from_int64
#define f32                 fa_from_double
#define f64                 fa_from_double

#define mark_used(X) X = *(&X)


#endif // _FA_UTIL

