
#ifndef _DOREMIR_STRING
#define _DOREMIR_STRING

#include <doremir/std.h>

/** @defgroup Doremir
    @{
    @defgroup String
    @{
    */

typedef char * doremir_string_t;
typedef uint8_t * doremir_string_utf8_t;
typedef uint16_t * doremir_string_utf16_t;
typedef uint32_t * doremir_string_utf32_t;
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

