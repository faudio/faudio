
#ifndef _FA_STRING
#define _FA_STRING

#include <fa.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaString String
    @{
    */

typedef fa_char8_t * fa_string_utf8_t;
typedef fa_char16_t * fa_string_utf16_t;
typedef fa_char32_t * fa_string_utf32_t;
typedef struct _fa_string_t * fa_string_t;
fa_string_t fa_string_empty();
fa_string_t fa_string_single(fa_char16_t);
fa_string_t fa_string_repeat(int, fa_char16_t);
fa_string_t fa_string_copy(fa_string_t);
fa_string_t fa_string_append(fa_string_t, fa_string_t);
fa_string_t fa_string_dappend(fa_string_t, fa_string_t);
void fa_string_destroy(fa_string_t);
int fa_string_length(fa_string_t);
fa_char16_t fa_string_char_at(int, fa_string_t);
typedef struct {
            fa_string_t (* show)(fa_ptr_t);
        } fa_string_show_t;
fa_string_t fa_string_show(fa_ptr_t);
fa_string_t fa_string_to_string(fa_ptr_t);
fa_string_t fa_string_to_json(fa_ptr_t);
fa_ptr_t fa_string_from_json(fa_string_t);
fa_string_utf8_t fa_string_to_utf8(fa_string_t);
fa_string_utf16_t fa_string_to_utf16(fa_string_t);
fa_ptr_t fa_string_to_native(fa_string_t);
fa_string_t fa_string_from_utf8(fa_string_utf8_t);
fa_string_t fa_string_from_utf16(fa_string_utf16_t);
fa_string_t fa_string_from_native(fa_ptr_t);
bool fa_string_matches(fa_string_t, fa_string_t);
fa_string_t fa_string_format_integral(char *, long);
fa_string_t fa_string_format_floating(char *, double);

/** @}
    @}
    */

#endif // _FA_STRING

