
#ifndef _DOREMIR_STRING
#define _DOREMIR_STRING

#include <doremir.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirString String
    @{
    */

typedef struct _doremir_string_t * doremir_string_t;
typedef doremir_string_t doremir_string_file_path_t;
doremir_string_t doremir_string_empty();
doremir_string_t doremir_string_single(char);
int doremir_string_length(doremir_string_t);
typedef struct {
            doremir_string_t (* show)(doremir_ptr_t);
        } doremir_string_show_t;

/** @}
    @}
    */

#endif // _DOREMIR_STRING

