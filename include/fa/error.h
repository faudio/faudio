
#ifndef _FA_ERROR
#define _FA_ERROR

#include <fa.h>
#include <fa/string.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaError Error
    @{
    */

typedef enum {
            info, warning, error, misc
        } fa_error_severity_t;
typedef struct {
            fa_error_severity_t (* severity)(fa_ptr_t);
            fa_string_t (* message)(fa_ptr_t);
            fa_string_t (* origin)(fa_ptr_t);
        } fa_error_interface_t;
typedef struct _fa_error_t * fa_error_t;
bool fa_error_check(fa_ptr_t);
void fa_error_log(fa_ptr_t, fa_error_t);
fa_error_severity_t fa_error_severity(fa_error_t);
fa_string_t fa_error_message(fa_error_t);
fa_string_t fa_error_origin(fa_error_t);
fa_string_t fa_error_format(bool, fa_error_t);
typedef void (* fa_error_callback_t)(fa_ptr_t, fa_error_t);
fa_error_t fa_error_create_simple(fa_error_severity_t,
                                  fa_string_t,
                                  fa_string_t);

/** @}
    @}
    */

#endif // _FA_ERROR

