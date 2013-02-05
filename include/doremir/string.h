
#ifndef _DOREMIR_STRING
#define _DOREMIR_STRING

#include <doremir.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirString String
    @{
    */

typedef doremir_char8_t * doremir_string_utf8_t;
typedef doremir_char16_t * doremir_string_utf16_t;
typedef doremir_char32_t * doremir_string_utf32_t;
typedef struct _doremir_string_t * doremir_string_t;
typedef doremir_string_t doremir_string_file_path_t;
typedef struct {
            doremir_string_t (* show)(doremir_ptr_t);
        } doremir_string_show_t;
typedef struct {
            doremir_string_t (* to_json)(doremir_ptr_t);
        } doremir_string_to_json_t;
typedef struct {
            doremir_ptr_t (* from_json)(doremir_string_t);
        } doremir_string_from_json_t;
doremir_string_t doremir_string_show(doremir_ptr_t);
doremir_string_t doremir_string_to_string(doremir_ptr_t);
doremir_string_t doremir_string_empty();
doremir_string_t doremir_string_single(doremir_char16_t);
doremir_string_t doremir_string_copy(doremir_string_t);
doremir_string_t doremir_string_append(doremir_string_t,
                                       doremir_string_t);
doremir_string_t doremir_string_dappend(doremir_string_t,
                                        doremir_string_t);
void doremir_string_destroy(doremir_string_t);
int doremir_string_length(doremir_string_t);
doremir_char16_t doremir_string_char_at(int, doremir_string_t);
doremir_string_utf8_t doremir_string_to_utf8(doremir_string_t);
doremir_string_utf16_t doremir_string_to_utf16(doremir_string_t);
doremir_string_utf32_t doremir_string_to_utf32(doremir_string_t);
void * doremir_string_to_cf_string(doremir_string_t);
doremir_string_t doremir_string_from_utf8(doremir_string_utf8_t);
doremir_string_t doremir_string_from_utf16(doremir_string_utf16_t);
doremir_string_t doremir_string_from_utf32(doremir_string_utf32_t);
doremir_string_t doremir_string_from_cf_string(void *);
doremir_string_t doremir_string_format_integer(char *, long);
doremir_string_t doremir_string_format_floating(char *, double);
doremir_string_t doremir_string_map(doremir_unary_t,
                                    doremir_ptr_t,
                                    doremir_string_t);
doremir_string_t doremir_string_join_map(doremir_unary_t,
                                         doremir_ptr_t,
                                         doremir_string_t);

/** @}
    @}
    */

#endif // _DOREMIR_STRING

