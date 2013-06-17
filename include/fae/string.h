
#ifndef _FAE_STRING
#define _FAE_STRING

#include <fae.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeString String
    @{
    */

typedef fae_char8_t * fae_string_utf8_t;
typedef fae_char16_t * fae_string_utf16_t;
typedef fae_char32_t * fae_string_utf32_t;
typedef struct _fae_string_t * fae_string_t;
typedef fae_string_t fae_string_file_path_t;
fae_string_t fae_string_empty();
fae_string_t fae_string_single(fae_char16_t);
fae_string_t fae_string_repeat(int, fae_char16_t);
fae_string_t fae_string_copy(fae_string_t);
fae_string_t fae_string_append(fae_string_t, fae_string_t);
fae_string_t fae_string_dappend(fae_string_t, fae_string_t);
void fae_string_destroy(fae_string_t);
int fae_string_length(fae_string_t);
fae_char16_t fae_string_char_at(int, fae_string_t);
typedef struct {
            fae_string_t (* show)(fae_ptr_t);
        } fae_string_show_t;
fae_string_t fae_string_show(fae_ptr_t);
fae_string_t fae_string_to_string(fae_ptr_t);
fae_string_t fae_string_to_json(fae_ptr_t);
fae_ptr_t fae_string_from_json(fae_string_t);
fae_string_utf8_t fae_string_to_utf8(fae_string_t);
fae_string_utf16_t fae_string_to_utf16(fae_string_t);
fae_string_utf32_t fae_string_to_utf32(fae_string_t);
fae_ptr_t fae_string_to_native(fae_string_t);
fae_string_t fae_string_from_utf8(fae_string_utf8_t);
fae_string_t fae_string_from_utf16(fae_string_utf16_t);
fae_string_t fae_string_from_utf32(fae_string_utf32_t);
fae_string_t fae_string_from_native(fae_ptr_t);
fae_string_t fae_string_format_integral(char *, long);
fae_string_t fae_string_format_floating(char *, double);

/** @}
    @}
    */

#endif // _FAE_STRING

