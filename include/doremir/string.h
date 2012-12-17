
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
doremir_string_t doremir_string_single(uint16_t);
doremir_string_t doremir_string_copy(doremir_string_t);
void doremir_string_destroy(doremir_string_t);
int doremir_string_length(doremir_string_t);
uint16_t doremir_string_char_at(int, doremir_string_t);
typedef struct {
            doremir_string_t (* show)(doremir_ptr_t);
        } doremir_string_show_t;
typedef char * doremir_string_utf8_t;
typedef uint16_t * doremir_string_utf16_t;
typedef uint32_t * doremir_string_utf32_t;
doremir_string_utf8_t doremir_string_to_utf8(doremir_string_t);
doremir_string_utf16_t doremir_string_to_utf16(doremir_string_t);
doremir_string_utf32_t doremir_string_to_utf32(doremir_string_t);
doremir_string_t doremir_string_from_utf8(doremir_string_utf8_t);
doremir_string_t doremir_string_from_utf16(doremir_string_utf16_t);
doremir_string_t doremir_string_from_utf32(doremir_string_utf32_t);

/** @}
    @}
    */

#endif // _DOREMIR_STRING

