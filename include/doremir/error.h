
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
        } doremir_error_category_t;
typedef struct {
            doremir_error_category_t (* severity)(doremir_ptr_t);
            doremir_string_t (* message)(doremir_ptr_t);
            doremir_string_t (* origin)(doremir_ptr_t);
        } doremir_error_t;
doremir_error_category_t doremir_error_severity(doremir_ptr_t);
doremir_string_t doremir_error_message(doremir_ptr_t);
doremir_string_t doremir_error_origin(doremir_ptr_t);
bool doremir_error_check(doremir_ptr_t);
void doremir_error_log(doremir_ptr_t, doremir_ptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_ERROR

