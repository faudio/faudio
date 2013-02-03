
#ifndef _DOREMIR_ERROR
#define _DOREMIR_ERROR

#include <doremir.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirError Error
    @{
    */

typedef enum {
            info, warning, error, misc
        } doremir_error_severity_t;
typedef struct {
            doremir_error_severity_t (* severity)(doremir_ptr_t);
            doremir_string_t (* message)(doremir_ptr_t);
            doremir_string_t (* origin)(doremir_ptr_t);
        } doremir_error_interface_t;
typedef struct _doremir_error_t * doremir_error_t;
typedef void (* doremir_error_callback_t)(doremir_ptr_t,
                                          doremir_error_t);
doremir_error_t doremir_error_create_simple(doremir_error_severity_t,
                                            doremir_string_t,
                                            doremir_string_t);
doremir_error_severity_t doremir_error_severity(doremir_error_t);
doremir_string_t doremir_error_message(doremir_error_t);
doremir_string_t doremir_error_origin(doremir_error_t);
void doremir_error_log(doremir_ptr_t, doremir_error_t);
doremir_string_t doremir_error_format(bool, doremir_error_t);
bool doremir_error_check(doremir_ptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_ERROR

