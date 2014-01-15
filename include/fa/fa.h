
#ifndef _FA_FA
#define _FA_FA

#include <fa/atomic.h>
#include <fa/atomic/queue.h>
#include <fa/atomic/stack.h>
#include <fa/atomic/ring_buffer.h>
#include <fa/system.h>
#include <fa/error.h>
#include <fa/pair.h>
#include <fa/pair/left.h>
#include <fa/list.h>
#include <fa/audio.h>
#include <fa/midi.h>
#include <fa/midi/message.h>
#include <fa/plot.h>
#include <fa/thread.h>
#include <fa/time.h>
#include <fa/priority_queue.h>
#include <fa/action.h>
#include <fa/signal.h>

/** @addtogroup FaFa

    Miscellaneous utilities.

    ~~~~
    #import <fa/fa.h>

    int main (int argc, char const *argv[])
    {
        fa_fa_initialize();

        ...

        fa_fa_terminate();
        return 0;
    }
    ~~~~

 
    @defgroup Fa Fa
    @{
    @defgroup FaFa Fa
    @{
    */

/** Returns the version of Fa as a list
    on the form `("alpha", 1, 0, 5, "")`.
*/
fa_list_t fa_fa_version();

/** Returns the version of Fa as a string
    on the form "alpha1.0.5".
*/
fa_string_t fa_fa_version_string();

/** Performs global initialization.

    This function must be called exactly once before any other function in the library.
    A call to fa_fa_terminate() will reset the global state so that
    fa_fa_initialize() may be called again and so on.
*/
void fa_fa_initialize();

/** Performs global cleanup.

    This function may be used to reset the global state as per above. It is not necessary to
    call this function before the program finishes.
*/
void fa_fa_terminate();


typedef void (* fa_fa_log_func_t)(fa_ptr_t,
                                  fa_time_system_t,
                                  fa_error_t);

/** Instruct Fa to write log messages to the specific file.
*/
void fa_fa_set_log_file(fa_string_t);

/** Instruct Fa to write log messages to the standard output.
*/
void fa_fa_set_log_std();

/** Instruct Fa to pass log messages to the given handler.
*/
void fa_fa_set_log(fa_fa_log_func_t, fa_ptr_t);

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref fa_error_interface_t).
*/
void fa_fa_log(fa_ptr_t, fa_error_t);

/** Write an informative message to the log.
*/
void fa_fa_log_info(fa_string_t);

/** Write a warning to the log.
*/
void fa_fa_log_warning(fa_string_t);

/** Write an error to the log.
*/
void fa_fa_log_error(fa_string_t);

/** Write an informative message to the log.
*/
void fa_fa_log_info_from(fa_string_t, fa_string_t);

/** Write a warning to the log.
*/
void fa_fa_log_warning_from(fa_string_t, fa_string_t);

/** Write an error to the log.
*/
void fa_fa_log_error_from(fa_string_t, fa_string_t);

/** @}
    @}
    */

#endif // _FA_FA

