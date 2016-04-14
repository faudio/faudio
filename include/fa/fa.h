
#ifndef _FA_UTILITY
#define _FA_UTILITY

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
#include <fa/thread.h>
#include <fa/time.h>
#include <fa/priority_queue.h>
#include <fa/action.h>
#include <fa/signal.h>

/** @addtogroup FaUtility

    Miscellaneous

    ~~~~
    #import <fa/fa.h>

    int main (int argc, char const *argv[])
    {
        fa_initialize();

        ...

        fa_terminate();
        return 0;
    }
    ~~~~

 
    @defgroup Fa Fa
    @{
    @defgroup FaUtility Utility
    @{
    */

/** Returns the version of Fa as a list
    on the form `("alpha", 1, 0, 5, "")`.
*/
fa_list_t fa_version();

/** Returns the version of Fa as a string
    on the form "alpha1.0.5".
*/
fa_string_t fa_version_string();

/** Performs global initialization.

    This function must be called exactly once before any other function in the library.
    A call to fa_terminate() will reset the global state so that
    fa_initialize() may be called again and so on.
*/
void fa_initialize();

/** Performs global cleanup.

    This function may be used to reset the global state as per above. It is not necessary to
    call this function before the program finishes.
*/
void fa_terminate();


typedef void (* fa_log_func_t)(fa_ptr_t,
                               fa_time_system_t,
                               fa_error_t);

/** Instruct Fa to write log messages to the specific file.
*/
void fa_set_log_file(fa_string_t string);

/** Instruct Fa to write log messages to the standard output.
*/
void fa_set_log_std();

/** Instruct Fa to write log messages to the specific file AND to stdout;
*/
void fa_set_log_file_and_stdout(fa_string_t string);

/** Instruct Fa to pass log messages to the given handler.
*/
void fa_set_log(fa_log_func_t logFunc, fa_ptr_t ptr);

/** Set minimum error level to log. Default is misc (everything is logged).
    Other valid values are info, warning and error.
*/
void fa_set_log_level(fa_error_severity_t level);

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref fa_error_interface_t).
*/
void fa_log(fa_ptr_t ptr, fa_error_t error);

/** Write an informative message to the log. The argument is destroyed.
*/
void fa_log_info(fa_string_t string);

/** Write a warning to the log. The argument is destroyed.
*/
void fa_log_warning(fa_string_t string);

/** Write an error to the log. The argument is destroyed.
*/
void fa_log_error(fa_string_t string);

/** Write an informative message to the log. The arguments are destroyed.
*/
void fa_log_info_from(fa_string_t string, fa_string_t string_);

/** Write a warning to the log. The arguments are destroyed.
*/
void fa_log_warning_from(fa_string_t string, fa_string_t string_);

/** Write an error to the log. The arguments are destroyed.
*/
void fa_log_error_from(fa_string_t string, fa_string_t string_);

/** Write the number of allocated regions to the log, with an optional string prepended. The argument is destroyed.
	 
*/
void fa_log_region_count(fa_string_t string);

/** @}
    @}
    */

#endif // _FA_UTILITY

