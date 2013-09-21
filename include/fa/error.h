
#ifndef _FA_ERROR
#define _FA_ERROR

#include <fa.h>
#include <fa/string.h>

/** @addtogroup FaError

    @addtogroup FaError
    
    Error handling types and functions.
 
    @defgroup Fa Fa
    @{
    @defgroup FaError Error
    @{
    */


typedef enum {
            info, warning, error, misc
        } fa_error_severity_t;

/** @interface fa_error_interface_t
    
    Error interface.

    Generally there is no need to implement this interface directly, the
    @ref fa_error_create_simple function returns an implementation
    suitable for most purposes.        
    
*/
typedef struct {
            fa_error_severity_t (* severity)(fa_ptr_t);
            fa_string_t (* message)(fa_ptr_t);
            fa_string_t (* origin)(fa_ptr_t);
        } fa_error_interface_t;

/** @typedef fa_error_t

    A type implementing the error interface.

    If you cast a value to this type, you must assure that it implements
    @ref fa_error_interface_t, conversely if you cast a value from
    this type you can assume that it implements @ref fa_error_t.
    
    You can check whether an arbitrary reference is an error using
    @ref fa_error_check.
*/
typedef struct _fa_error_t * fa_error_t;


bool fa_error_check(fa_ptr_t);


void fa_error_log(fa_ptr_t, fa_error_t);


fa_error_severity_t fa_error_severity(fa_error_t);


fa_string_t fa_error_message(fa_error_t);


fa_string_t fa_error_origin(fa_error_t);


fa_string_t fa_error_format(bool, fa_error_t);

/** @typedef fa_error_callback_t
    
    An error handler routine.
*/
typedef void (* fa_error_callback_t)(fa_ptr_t, fa_error_t);


fa_error_t fa_error_create_simple(fa_error_severity_t,
                                  fa_string_t,
                                  fa_string_t);

/** @}
    @}
    */

#endif // _FA_ERROR

