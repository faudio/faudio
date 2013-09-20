
#ifndef _FA_FA
#define _FA_FA

#include <fa/atomic.h>
#include <fa/atomic/queue.h>
#include <fa/atomic/stack.h>
#include <fa/atomic/ring_buffer.h>
#include <fa/system.h>
#include <fa/error.h>
#include <fa/graph.h>
#include <fa/audio.h>
#include <fa/midi.h>
#include <fa/midi/message.h>
#include <fa/plot.h>
#include <fa/thread.h>
#include <fa/time.h>
#include <fa/priority_queue.h>
#include <fa/signal.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaFa Fa
    @{
    */

fa_list_t fa_fa_version();
fa_string_t fa_fa_version_string();
void fa_fa_initialize();
void fa_fa_terminate();
typedef void (* fa_fa_log_func_t)(fa_ptr_t,
                                  fa_time_system_t,
                                  fa_error_t);
void fa_fa_set_log_file(fa_string_t);
void fa_fa_set_log_std();
void fa_fa_set_log(fa_fa_log_func_t, fa_ptr_t);
void fa_fa_log(fa_ptr_t, fa_error_t);
void fa_fa_log_info(fa_string_t);
void fa_fa_log_warning(fa_string_t);
void fa_fa_log_error(fa_string_t);
void fa_fa_log_info_from(fa_string_t, fa_string_t);
void fa_fa_log_warning_from(fa_string_t, fa_string_t);
void fa_fa_log_error_from(fa_string_t, fa_string_t);
void fa_fa_dlog(fa_ptr_t, fa_error_t);
void fa_fa_dlog_info(fa_string_t);

/** @}
    @}
    */

#endif // _FA_FA

