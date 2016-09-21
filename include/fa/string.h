
#ifndef _FA_STRING
#define _FA_STRING

#include <fa.h>

/** @addtogroup FaString
 
    Provides strings.
    
    A string is an immutable sequence of Unicode characters with single-ownership semantics.
    To convert a string from and to its ordinary C representation, use @ref fa_string_from_utf8,
    @ref fa_string_to_utf8, or the utility wrappers `string` and `unstring`.

    For example:
    
    ~~~c
    char*       cstr  = "Hello";
    string_t    str   = string(cstr);
    string_t    str2  = fa_string_dappend(str, string(" world!"));

    printf("%s\n", unstring(str2));
    ~~~

    @par Literals
    - `fa_string("foo")`

    @par Implements 
    - fa_equal_t
    - fa_order_t
    - fa_copy_t
    - fa_destroy_t
    - fa_dynamic_t
    - fa_string_show_t
    
    @see
    - [Data structures](@ref DataStructures)
    
 
    @defgroup Fa Fa
    @{
    @defgroup FaString String
    @{
    */

/**
    @typedef fa_string_t
    An immutable Unicode string.

    @typedef fa_string_file_path_t
    A file path.
*/
typedef struct _fa_string_t * fa_string_t;

typedef enum fa_string_allocation_t 
{
    kSaHeap    = 0,
    kSaLiteral = 1,
    kSaStack   = 2
}  fa_string_allocation_t;

/** Create an empty string.

    The returned string should be destroyed by the caller.
*/
fa_string_t fa_string_empty();

/** Create a single-char string.

    The returned string should be destroyed by the caller.
*/
fa_string_t fa_string_single(fa_char8_t char8);

/** Create a string by repeating the given character.

    The returned string should be destroyed by the caller.
*/
fa_string_t fa_string_repeat(int int_, fa_char8_t char8);

/** Copy the given string.

    The returned string should be destroyed by the caller.
*/
fa_string_t fa_string_copy(fa_string_t string);

/** Append the given strings, which are both copied.

    The returned string should be destroyed by the caller.
*/
fa_string_t fa_string_append(fa_string_t string,
                             fa_string_t string_);

/** Append the given strings, which are both destroyed.

    The returned string should be destroyed by the caller.
*/
fa_string_t fa_string_dappend(fa_string_t string,
                              fa_string_t string_);

/** Destroy the given string.
*/
void fa_string_destroy(fa_string_t string);

/** Return the number of characters in the given string.
*/
int fa_string_length(fa_string_t string);

/** Return the character at the given position in the string.
    @param pos
    @param str
*/
fa_char16_t fa_string_char_at(int index, fa_string_t string);

/** String conversion interface.
    
*/
typedef struct {
            fa_string_t (* show)(fa_ptr_t);
        } fa_string_show_t;

/** Generic string conversion.

    @param value
        Value to convert.
    @return
        A new string.
*/
fa_string_t fa_string_show(fa_ptr_t ptr);

/** Generic destructive string conversion.

    @param value
        Value to convert. The value is destroyed.
    @return
        A new string.
*/
fa_string_t fa_string_dshow(fa_ptr_t ptr);

/** Behaves like the identity function on strings and as [show](@ref fa_string_show)
    on all other value.
    @see [Show](@ref fa_string_show_t)
      
*/
fa_string_t fa_string_to_string(fa_ptr_t ptr);

/** Generic JSON conversion.
    @param a    Value implementing [Show](@ref fa_string_show_t) or [Dynamic](@ref fa_string_dynamic_t).
      
*/
fa_string_t fa_string_to_json(fa_ptr_t ptr);

/** Generic JSON conversion.
    @param string   A JSON string.
      
*/
fa_ptr_t fa_string_from_json(fa_string_t string);

/** Encode the given string as UTF-8.

    @param  str String to encode.
    @return
    qA new encoded string.
*/
fa_char8_t* fa_string_to_utf8(fa_string_t string);

/** Encode a string as CP1252, also known as the standard Windows charset.

    @param  str String to encode.
    @return
        A new encoded string.
*/
fa_char8_t* fa_string_to_cp1252(fa_string_t string);

/** Encode the given string as UTF-16.

    @param  str String to encode.
    @return
        A heap-allocated encoded string.
*/
fa_char16_t* fa_string_to_utf16(fa_string_t string);

/** Convert a string to a the string representation used by the platform.

    * On Mac OS X and iOS, `CFStringRef` is used.
*/
fa_ptr_t fa_string_to_native(fa_string_t string);

/** Convert a string to a "wide string", i.e. a wchar_t*
    Native conversion functions are used.
    (Currently only available on Windows)
*/
wchar_t* fa_string_to_wstr(fa_string_t string);

/** Deencode a string from UTF-8.

    @param  str Encoded string.
    @return
        A new string.
*/
fa_string_t fa_string_from_utf8(const fa_char8_t* utf8);

/** Create a string from a c literal. Memory and CPU efficient, but no error checking is made.
    Please use the fa_string macro instead!

    @param  cstr A c literal string.
    @param  size The size of the string (not including NUL-termination)
    @return
        A new string.
*/
fa_string_t fa_string_from_literal(const char* cstr, size_t size);

/** Deencode a string from CP1252, also known as the standard Windows charset.

    @param  str Encoded string.
    @return
        A new string.
*/
fa_string_t fa_string_from_cp1252(const fa_char8_t* cp1252);

/** Deencode a string from Mac OS X Roman, also known as the standard Windows charset.

    @param  str Encoded string.
    @return
        A new string.
*/
fa_string_t fa_string_from_mac_roman(const fa_char8_t* macRoman);

/** Deencode a string from UTF-16.

    @param  str Encoded string.
    @return
        A new string.
*/
fa_string_t fa_string_from_utf16(const fa_char16_t* utf16);

/** Convert a value of the string representation used by the platform to a string.

    * On Mac OS X and iOS, `CFStringRef` is used.
*/
fa_string_t fa_string_from_native(fa_ptr_t ptr);

/** Return true iff the given string matches the given regular expression.
    @param expr   A regular expression string.
    @param string String to match.
*/
bool fa_string_matches(fa_string_t string, fa_string_t string_);

/** Format an integer.
    @param format
        A printf-style format string.
    @param value
        Integer value.
    @return
        A new formatted string.
*/
fa_string_t fa_string_format_integral(const char *, long long_);

/** Format a floating-point value.
    @param format
        A printf-style format string.
    @param value
        Numeric value.
    @return
        A new formatted string.
*/
fa_string_t fa_string_format_floating(const char *, double double_);

/** printf-style formatting.
    @param format
        A printf-style format string.
    @param ...
        Values matching the format string.
    @return
        A new formatted string.
*/
fa_string_t fa_string_format(const char *, ...) __attribute__((format(printf,1,2)));


void fa_string_log_count();

/** @}
    @}
    */

#endif // _FA_STRING

