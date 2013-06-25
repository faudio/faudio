
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/string.h>
#include <fae/util.h>
#include <fae/dynamic.h>
#include <iconv.h>

#include "string/trex.h"
#include "string/parson.h"

/*
    Notes:
        * Straightforward implementation using bounded buffer
        * Slow copying
        * Reasonable dappend (using realloc)

    Possibilities:
        * More efficient append
            * Separate size from char count and pre-allocate (such as 2^n).
        * Real-time alternative?
            * Non-amortized allocator?
            * Regional string buffer/symbol table solution?
 */

#define standard_code_k   "UTF-16LE"          // Internal string code
#define char_size_k       sizeof(uint16_t)    // Internal char size

struct _fae_string_t {
    impl_t          impl;           // Dispatcher
    size_t          size;           // Character count
    uint16_t       *data;           // Payload
};


// --------------------------------------------------------------------------------

static void          string_fatal(char *msg, int error);
static fae_ptr_t string_impl(fae_id_t interface);

string_t new_string(size_t size, uint16_t *data)
{

    string_t str = fae_new(string);

    str->impl = &string_impl;
    str->size = size;
    str->data = data;

    return str;
}

void delete_string(string_t str)
{
    fae_delete(str);
}

// --------------------------------------------------------------------------------

/** Create an empty string.

    The returned string should be destroyed by the caller.
 */
fae_string_t fae_string_empty()
{
    return new_string(0, NULL);
}

/** Create a single-char string.

    The returned string should be destroyed by the caller.
 */
fae_string_t fae_string_single(fae_char16_t chr)
{
    string_t str = new_string(1, NULL);
    str->data = malloc(char_size_k);
    str->data[0] = chr;

    return str;
}

/** Create a string by repeating the given character.

    The returned string should be destroyed by the caller.
 */
fae_string_t fae_string_repeat(int times, fae_char16_t chr)
{
    string_t s = string("");

    for (int i = 0; i < times; ++i) {
        write_to(s, fae_string_single(chr));
    }

    return s;
}


/** Copy the given string.

    The returned string should be destroyed by the caller.
 */
fae_string_t fae_string_copy(fae_string_t str)
{
    string_t pst = new_string(str->size, NULL);
    pst->data = malloc(str->size * char_size_k);

    memcpy(pst->data, str->data, str->size * char_size_k);

    return pst;
}

/** Append the given strings.

    The returned string should be destroyed by the caller.
 */
fae_string_t fae_string_append(fae_string_t str1,
                               fae_string_t str2)
{
    string_t cs = new_string(str1->size + str2->size, NULL);
    cs->data = malloc(cs->size * char_size_k);

    memcpy(cs->data, str1->data, str1->size * char_size_k);
    memcpy(cs->data + str1->size, str2->data, str2->size * char_size_k);

    return cs;
}

/** Append the given strings, which are both destroyed.

    The returned string should be destroyed by the caller.
 */
fae_string_t fae_string_dappend(fae_string_t str1,
                                fae_string_t str2)
{
    size_t oldSize = str1->size;

    str1->size = str1->size + str2->size;
    str1->data = realloc(str1->data, str1->size * char_size_k);

    memcpy(str1->data + oldSize, str2->data, str2->size * char_size_k);

    free(str2);
    return str1;
}

/** Destroy the given string.
 */
void fae_string_destroy(fae_string_t str)
{
    free(str->data);
    delete_string(str);
}

/** Return the number of characters in the given string.
 */
int fae_string_length(fae_string_t str)
{
    return str->size;
}

/** Return the character at the given position in the string.
    @param pos
    @param str
 */
uint16_t fae_string_char_at(int pos, fae_string_t str)
{
    if (pos < 0 || pos >= str->size) {
        assert(false && "Character out of range");
    }

    return str->data[pos];
}


// --------------------------------------------------------------------------------

/** Format an integer.
    @param format
        A printf-style format string.
    @param value
        Integer value.
    @return
        A new formatted string.
 */
fae_string_t fae_string_format_integral(char *format, long value)
{
    char buffer[100];
    int  numChars;

    numChars = snprintf(buffer, 100, format, value);

    if (numChars > 100) {
        string_fatal("Too many characters", -1);
    }

    buffer[numChars] = 0;
    return fae_string_from_utf8(buffer);
}

/** Format a floating-point value.
    @param format
        A printf-style format string.
    @param value
        Numeric value.
    @return
        A new formatted string.
 */
fae_string_t fae_string_format_floating(char *format, double value)
{
    char buffer[100];
    int  numChars;

    numChars = snprintf(buffer, 100, format, value);

    if (numChars > 100) {
        string_fatal("Too many characters", -1);
    }

    buffer[numChars] = 0;
    return fae_string_from_utf8(buffer);
}


// --------------------------------------------------------------------------------

/** Fail with error message, interpreting errno as an iconv error.

    This function does not return.
 */
static inline void iconv_fail()
{
    switch (errno) {
    case E2BIG:
        string_fatal("iconv: Output buffer too small",
                     errno);

    case EILSEQ:
        string_fatal("iconv: Input byte does not belong to the input codeset",
                     errno);

    case EINVAL:
        string_fatal("iconv: Incomplete character or shift sequence at the end of the input buffer",
                     errno);

    default:
        string_fatal("iconv: Unknown error",
                     errno);
    }
}

static inline size_t raw_size(char *s)
{
    size_t i = 0;

    while (s[i]) {
        i++;
    }

    return i;
}

static inline size_t raw_size_16(uint16_t *s)
{
    size_t i = 0;

    while (s[i]) {
        i++;
    }

    return i;
}

/** Encode the given string as UTF-8.

    @param  str String to encode.
    @return
        A heap-allocated encoded string.
 */
fae_string_utf8_t fae_string_to_utf8(fae_string_t str)
{
    size_t inSize, outSize, cstrSize;
    char *in, *out, *cstr;

    inSize  = str->size * char_size_k;   // exact char count
    outSize = str->size * 4;            // worst case, we shrink after iconv
    in      = (char *) str->data;
    out     = malloc(outSize);
    cstr    = out;

    {
        iconv_t conv   = iconv_open("UTF-8", standard_code_k);
        size_t  status = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        if (status == ((size_t) - 1)) {
            iconv_fail();
        }
    }

    cstrSize = out - cstr;
    cstr     = realloc(cstr, cstrSize + 1);

    cstr[cstrSize] = 0;                 // add null-terminator

    return cstr;
}

/** Encode the given string as UTF-16.

    @param  str String to encode.
    @return
        A heap-allocated encoded string.
 */
fae_string_utf16_t fae_string_to_utf16(fae_string_t as)
{
    size_t size = as->size;
    uint16_t *cstr = malloc((size + 1) * char_size_k);
    memcpy(cstr, as->data, as->size * char_size_k);
    cstr[size] = 0;
    return cstr;
}

/** Encode the given string as UTF-32.

    @param  str String to encode.
    @return
        A heap-allocated encoded string.
 */
fae_string_utf32_t fae_string_to_utf32(fae_string_t str)
{
    assert(false && "Not implemented");
}

/** Deencode a string from UTF-8.

    @param  str Encoded string.
    @return
        A new string.
 */
fae_string_t fae_string_from_utf8(fae_string_utf8_t cstr)
{
    size_t inSize, outSize, strSize;
    char *in, *out, *str;

    inSize  = raw_size(cstr);    // char count is in [inSize/4,inSize]
    outSize = inSize * 2;        // worst case, we shrink after iconv
    in      = cstr;
    out     = malloc(outSize);
    str     = out;

    {
        iconv_t conv = iconv_open(standard_code_k, "UTF-8");
        size_t status = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        if (status == ((size_t) - 1)) {
            iconv_fail();
        }
    }

    strSize = out - str;
    str     = realloc(str, strSize);

    return new_string(strSize / char_size_k, (uint16_t *) str);
}

/** Deencode a string from UTF-16.

    @param  str Encoded string.
    @return
        A new string.
 */
fae_string_t fae_string_from_utf16(fae_string_utf16_t cstr)
{
    size_t size = raw_size_16(cstr);
    string_t as = new_string(size, malloc(size * char_size_k));
    memcpy(cstr, as->data, as->size * char_size_k);
    return as;
}

/** Deencode a string from UTF-32.

    @param  str Encoded string.
    @return
        A new string.
 */
fae_string_t fae_string_from_utf32(fae_string_utf32_t cstr)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

/** Convert the given value to a string.
    @see [Show](@ref fae_string_show_t)
  */
fae_string_t fae_string_show(fae_ptr_t a)
{
    assert(fae_interface(fae_string_show_i, a) && "Must implement Show");
    return ((fae_string_show_t *) fae_interface(fae_string_show_i, a))->show(a);
}

/** Behaves like the identity function on strings and as [show](@ref fae_string_show)
    on all other value.
    @see [Show](@ref fae_string_show_t)
  */
fae_string_t fae_string_to_string(fae_ptr_t a)
{
    assert(fae_interface(fae_string_show_i, a) && "Must implement Show");

    bool is_string = fae_interface(fae_dynamic_i, a)
                     && (fae_dynamic_get_type(a) == string_type_repr);

    if (is_string) {
        return fae_string_copy(a);
    } else {
        return fae_string_show(a);
    }
}


inline static ptr_t jsonify(ptr_t a)
{
    switch (fae_dynamic_get_type(a)) {
    case pair_type_repr:
        return jsonify(fae_pair_to_list(a));

    case set_type_repr:
        return jsonify(fae_set_to_list(a));

    case list_type_repr:
        return fae_list_map(apply1, jsonify, a);

    case map_type_repr:
        return fae_map_map(apply1, jsonify, a);

    default:
        return a;
    }
}

/** Generic JSON conversion.
    @param a    Value implementing [Show](@ref fae_string_show_t) or [Dynamic](@ref fae_string_dynamic_t).
  */
fae_string_t fae_string_to_json(fae_ptr_t a)
{
    if (!fae_interface(fae_dynamic_i, a)) {
        return fae_string_show(a);
    } else {
        return fae_string_show(jsonify(a));
    }
}

ptr_t unjsonify(JSON_Value *a, bool *ok)
{
    switch (json_value_get_type(a)) {
    case JSONError:
        *ok = false;
        return NULL;

    case JSONNull:
        return fae_list_empty();

    case JSONString:
        return string((char *) json_value_get_string(a));

    case JSONNumber:
        return i32(json_value_get_number(a));

    case JSONBoolean:
        return fb(json_value_get_boolean(a));

    case JSONArray: {
        JSON_Array *ar  = json_value_get_array(a);
        size_t sz       = json_array_get_count(ar);
        list_t list     = fae_list_empty();

        for (size_t i = sz; i > 0; --i) {
            ptr_t v = unjsonify(json_array_get_value(ar, i - 1), ok);

            if (!ok) {
                return NULL;
            }

            list = fae_list_dcons(v, list);
        }

        return list;
    }

    case JSONObject: {
        JSON_Object *obj = json_value_get_object(a);
        size_t sz = json_object_get_count(obj);
        map_t map = fae_map_empty();

        for (size_t i = 0; i < sz; ++i) {
            char *name = (char *) json_object_get_name(obj, i);
            ptr_t value = unjsonify(json_object_get_value(obj, name), ok);

            if (!ok) {
                return NULL;
            }

            map = fae_map_dset(string(name), value, map);
        }

        return map;
    }

    default:
        assert(false && "Missing case");
    }
}

/** Generic JSON conversion.
    @param string   A JSON string.
  */
fae_ptr_t fae_string_from_json(fae_string_t string)
{
    bool ok = true;
    ptr_t result = unjsonify(json_parse_string(unstring(string)), &ok);

    if (!ok) {
        assert(false && "Malformed JSON");    // TODO
    } else {
        return result;
    }
}


// --------------------------------------------------------------------------------

/** Return true iff the given string matches the given regular expression.
    @param expr   A regular expression string.
    @param string String to match.
 */
bool fae_string_matches(fae_string_t expr, fae_string_t string)
{
    if (fae_string_length(expr) <= 0) {
        return false;
    }

    for (int i = 0; i < expr->size; ++i) {
        if (expr->data[i] > 127) {
            assert(false && "Can not handle Unicode chars in expression.");
        }
    }

    char *cexpr    = fae_string_to_utf8(expr);
    char *cinput   = fae_string_to_utf8(string);

    TRex *exp = trex_compile(cexpr, NULL);
    bool res = trex_match(exp, cinput);

    trex_free(exp);
    free(cexpr);
    free(cinput);

    return res;
}


// --------------------------------------------------------------------------------

/** Return the result of applying the given function to all characters of the
    given string.

    @par Laws

        map(apply1, id, xs)                == xs
        map(apply1, f, map(apply1, g, xs)) == map(apply1, comp(f, g), xs)

    @par Performance
        O(n)
 */
// string_t fae_string_map(unary_t func, ptr_t data, string_t string)
// {
//     string_t result = fae_string_copy(string);
//     for (int i = 0; i < string->size; ++i)
//     {
//         result->data[i] = (uint16_t) (int32_t) func(data, (ptr_t) (int32_t) string->data[i]);
//     }
//     return result;
// }

/** Map over the given string and join the results.

    This function is useful to apply functions from single characters to strings.

    @par Laws

        joinMap(apply1, single, xs) == xs`

    @par Performance
        O(n)
 */
string_t fae_string_join_map(unary_t func, ptr_t data, string_t string)
{
    string_t result = string("");

    for (int i = 0; i < string->size; ++i) {
        result = string_dappend(result,

                                func(data, (ptr_t)(long) string->data[i]));
    }

    return result;
}


// --------------------------------------------------------------------------------

inline static string_t escape_char(uint16_t c)
{
    switch (c) {
    case '"':
        return string("\\\"");

    case '\\':
        return string("\\\\");

    default:
        return fae_string_single(c);
    }
}

inline static string_t escape(string_t string)
{
    return fae_string_join_map(apply1, escape_char, string);
}

static bool string_equal(fae_ptr_t as, fae_ptr_t bs)
{
    string_t cs, ds;
    cs = (string_t) as;
    ds = (string_t) bs;

    if (cs->size != ds->size) {
        return false;
    } else {
        for (size_t i = 0;
                i < cs->size && i < ds->size;
                ++i) {
            if (cs->data[i] != ds->data[i]) {
                return false;
            }
        }

        return true;
    }
}

#define pred(a) (a - 1)
#define last_elem(v) v->data[pred(v->size)]

static bool string_less_than(fae_ptr_t as, fae_ptr_t bs)
{
    string_t cs, ds;
    cs = (string_t) as;
    ds = (string_t) bs;

    for (size_t i = 0;
            i < pred(size_min(cs->size, ds->size));
            ++i) {
        if (cs->data[i] < ds->data[i]) {
            return true;
        }

        if (cs->data[i] > ds->data[i]) {
            return false;
        }
    }

    if (cs->size == ds->size) {
        return last_elem(cs) < last_elem(ds);
    } else {
        return (cs->size < ds->size);
    }
}

static bool string_greater_than(fae_ptr_t as, fae_ptr_t bs)
{
    string_t cs, ds;
    cs = (string_t) as;
    ds = (string_t) bs;

    for (size_t i = 0;
            i < pred(size_min(cs->size, ds->size));
            ++i) {
        if (cs->data[i] > ds->data[i]) {
            return true;
        }

        if (cs->data[i] < ds->data[i]) {
            return false;
        }
    }

    if (cs->size == ds->size) {
        return last_elem(cs) > last_elem(ds);
    } else {
        return (cs->size > ds->size);
    }
}


static fae_string_t string_show(fae_ptr_t a)
{
    string_t s = string("");
    s = string_dappend(s, string("\""));
    s = string_dappend(s, escape(a));
    s = string_dappend(s, string("\""));
    return s;
}


fae_ptr_t string_copy(fae_ptr_t a)
{
    return fae_string_copy(a);
}

void string_destroy(fae_ptr_t a)
{
    fae_string_destroy(a);
}

type_repr_t string_get_type(fae_ptr_t a)
{
    return string_type_repr;
}

fae_ptr_t string_impl(fae_id_t interface)
{
    static fae_equal_t string_equal_impl = { string_equal };
    static fae_copy_t string_copy_impl = { string_copy };
    static fae_string_show_t string_show_impl = { string_show };
    static fae_destroy_t string_destroy_impl = { string_destroy };
    static fae_order_t string_order_impl = { string_less_than, string_greater_than };
    static fae_dynamic_t string_dynamic_impl = { string_get_type };

    switch (interface) {
    case fae_equal_i:
        return &string_equal_impl;

    case fae_order_i:
        return &string_order_impl;

    case fae_string_show_i:
        return &string_show_impl;

    case fae_copy_i:
        return &string_copy_impl;

    case fae_destroy_i:
        return &string_destroy_impl;

    case fae_dynamic_i:
        return &string_dynamic_impl;

    default:
        return NULL;
    }
}

void string_fatal(char *msg, int error)
{
    void fae_fae_log_error_from(fae_string_t msg, fae_string_t origin);

    fae_fae_log_error_from(string(msg), string("Doremir.String"));
    exit(error);
}
