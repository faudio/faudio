
#ifndef _FAE_ERROR
#define _FAE_ERROR

#include <fae.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeError Error
    @{
    */

typedef enum {
            info, warning, error, misc
        } fae_error_severity_t;
typedef struct {
            fae_error_severity_t (* severity)(fae_ptr_t);
            fae_string_t (* message)(fae_ptr_t);
            fae_string_t (* origin)(fae_ptr_t);
        } fae_error_interface_t;
typedef struct _fae_error_t * fae_error_t;
bool fae_error_check(fae_ptr_t);
void fae_error_log(fae_ptr_t, fae_error_t);
fae_error_severity_t fae_error_severity(fae_error_t);
fae_string_t fae_error_message(fae_error_t);
fae_string_t fae_error_origin(fae_error_t);
fae_string_t fae_error_format(bool, fae_error_t);
typedef void (* fae_error_callback_t)(fae_ptr_t, fae_error_t);
fae_error_t fae_error_create_simple(fae_error_severity_t,
                                    fae_string_t,
                                    fae_string_t);

/** @}
    @}
    */

#endif // _FAE_ERROR

