
#ifndef _FA_ERROR
#define _FA_ERROR

#include <fa.h>
#include <fa/string.h>

/** @addtogroup FaError

    Error handling types and functions.
 
    @defgroup Fa Fa
    @{
    @defgroup FaError Error
    @{
    */


typedef enum {
            info, warning, error, misc
        } fa_error_severity_t;

/** Error interface.

    Generally there is no need to implement this interface directly, the
    @ref fa_error_create_simple function returns an implementation
    suitable for most purposes.        
    
*/
typedef struct {
            fa_error_severity_t (* severity)(fa_ptr_t);
            fa_string_t (* message)(fa_ptr_t);
            fa_string_t (* origin)(fa_ptr_t);
        } fa_error_interface_t;

/** A type implementing @ref fa_error_interface_t.

    If you cast a value to this type, you must assure that it implements
    @ref fa_error_interface_t, conversely if you cast a value from
    this type you can assume that it implements @ref fa_error_t.
    
    You can check whether an arbitrary reference is an error using
    @ref fa_error_check.
*/
typedef struct _fa_error_t * fa_error_t;

/** Return whether the given value is an error or not.

    This function is often used with [log](@ref fa_error_log) as in:

    ~~~
    if (fa_check(value)) {
        fa_error_log(NULL, value);
        exit(-1);
    }
    ~~~

    @param value Value to check (can be any type).
    @return
      A boolean.
*/
bool fa_error_check(fa_ptr_t ptr);

/** Write a log message.

    @param context
        Ignored, declared for compability with user-defined callbacks.
    @param error
        Condition to log. Must implement [Error](@ref fa_error_interface_t).
*/
void fa_error_log(fa_ptr_t ptr, fa_error_t error);

/** Return the severity of the given error.
*/
fa_error_severity_t fa_error_severity(fa_error_t error);

/** Return the message of the given error.
*/
fa_string_t fa_error_message(fa_error_t error);

/** Return the origin of the given error.
*/
fa_string_t fa_error_origin(fa_error_t error);

/** Convert the given error to a formated string.
    @param colored Include color escapes for terminals.
*/
fa_string_t fa_error_format(bool bool_, fa_error_t error);

/** An error handler routine.
*/
typedef void (* fa_error_callback_t)(fa_ptr_t, fa_error_t);

/** Creates a simple error.
    @param severity     Severity of the error.
    @param message      Error message.
    @param origin       Error origin (typically module name).
    @return
        A value of some type implementing
            [Error](@ref fa_error_interface_t),
            [Copy](@ref fa_copy_t) and
            [Destroy](@ref fa_destroy_t)
    @note
        Consumes the argument strings.
*/
fa_error_t fa_error_create_simple(fa_error_severity_t severity,
                                  fa_string_t string,
                                  fa_string_t string_);

/** @}
    @}
    */

#endif // _FA_ERROR

