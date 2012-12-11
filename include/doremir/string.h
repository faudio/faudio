
#ifndef _DOREMIR_STRING
#define _DOREMIR_STRING

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirString String
    @{
    */

typedef char * doremir_string_t;
typedef doremir_string_t doremir_string_file_path_t;
doremir_string_t doremir_string_create();
int doremir_string_length(doremir_string_t);
int doremir_string_length(doremir_string_t);
typedef char (* doremir_string_unary_t)(char);
typedef char (* doremir_string_binary_t)(char, char);
doremir_string_t doremir_string_map(doremir_string_t,
                                    doremir_string_unary_t);
doremir_string_t doremir_string_fold(doremir_string_t,
                                     doremir_string_binary_t,
                                     char);

/** @}
    @}
    */

#endif // _DOREMIR_STRING

