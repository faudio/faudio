
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
int doremir_string_length(doremir_string_t);
typedef char (* doremir_string_nullary_t)();
typedef char (* doremir_string_unary_t)(char);
typedef char (* doremir_string_binary_t)(char, char);
doremir_string_t doremir_string_map(doremir_string_t,
                                    doremir_string_unary_t);
doremir_string_t doremir_string_reduce(doremir_string_t,
                                       doremir_string_nullary_t,
                                       doremir_string_binary_t,
                                       char);
doremir_string_t doremir_string_map_reduce(doremir_string_t,
                                           doremir_string_nullary_t,
                                           doremir_string_unary_t,
                                           doremir_string_binary_t,
                                           char);
typedef doremir_string_t (* doremir_string_show_t)(doremir_ptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_STRING

