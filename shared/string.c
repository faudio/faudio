
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/string.h>
#include <fa/error.h>
#include <fa/dynamic.h>
#include <fa/util.h>

#include "string/trex.h"
#include "string/parson.h"

#include <iconv.h>

/*
    ## Notes

    * Straightforward implementation using bounded buffer

        - Only 16-bit, extended range not supported

        - Slow copying

        - Reasonable dappend (using realloc)
 */

#define kStandardCode       "UTF-16LE"          // Internal string code
#define kStandardCodeSize   sizeof(uint16_t)    // Internal char size

struct _fa_string_t {
    impl_t          impl;           // Dispatcher
    size_t          size;           // Character count
    uint16_t       *data;           // Payload
};


// --------------------------------------------------------------------------------

fa_error_t string_error(string_t msg);
static void string_fatal(char *msg, int error);
static fa_ptr_t string_impl(fa_id_t interface);

string_t new_string(size_t size, uint16_t *data)
{

    string_t str = fa_new(string);

    str->impl = &string_impl;
    str->size = size;
    str->data = data;

    return str;
}

void delete_string(string_t str)
{
    fa_delete(str);
}

// --------------------------------------------------------------------------------

fa_string_t fa_string_empty()
{
    return new_string(0, NULL);
}

fa_string_t fa_string_single(fa_char16_t chr)
{
    string_t str = new_string(1, NULL);
    str->data = fa_malloc(kStandardCodeSize);
    str->data[0] = chr;

    return str;
}

fa_string_t fa_string_repeat(int times, fa_char16_t chr)
{
    string_t s = string("");

    for (int i = 0; i < times; ++i) {
        fa_append_to(s, fa_string_single(chr));
    }

    return s;
}

fa_string_t fa_string_copy(fa_string_t str)
{
    string_t pst = new_string(str->size, NULL);
    pst->data = fa_malloc(str->size * kStandardCodeSize);

    memcpy(pst->data, str->data, str->size * kStandardCodeSize);

    return pst;
}

fa_string_t fa_string_append(fa_string_t str1,
                             fa_string_t str2)
{
    string_t cs = new_string(str1->size + str2->size, NULL);
    cs->data = fa_malloc(cs->size * kStandardCodeSize);

    memcpy(cs->data, str1->data, str1->size * kStandardCodeSize);
    memcpy(cs->data + str1->size, str2->data, str2->size * kStandardCodeSize);

    return cs;
}

fa_string_t fa_string_dappend(fa_string_t str1,
                              fa_string_t str2)
{
    size_t oldSize = str1->size;

    str1->size = str1->size + str2->size;
    str1->data = fa_realloc(str1->data, str1->size * kStandardCodeSize);

    memcpy(str1->data + oldSize, str2->data, str2->size * kStandardCodeSize);

    fa_free(str2);
    return str1;
}

void fa_string_destroy(fa_string_t str)
{
    fa_free(str->data);
    delete_string(str);
}

int fa_string_length(fa_string_t str)
{
    return str->size;
}

uint16_t fa_string_char_at(int pos, fa_string_t str)
{
    if (pos < 0 || pos >= str->size) {
        assert(false && "Character out of range");
    }

    return str->data[pos];
}

fa_string_t fa_string_format_integral(char *format, long value)
{
    char buffer[100];
    int  numChars;

    numChars = snprintf(buffer, 100, format, value);

    if (numChars > 100) {
        string_fatal("Too many characters", -1);
    }

    buffer[numChars] = 0;
    return fa_string_from_utf8(buffer);
}

fa_string_t fa_string_format_floating(char *format, double value)
{
    char buffer[100];
    int  numChars;

    numChars = snprintf(buffer, 100, format, value);

    if (numChars > 100) {
        string_fatal("Too many characters", -1);
    }

    buffer[numChars] = 0;
    return fa_string_from_utf8(buffer);
}


/** Fail with error message, interpreting errno as an iconv error.

    This function does not return.
 */
inline static
void iconv_fail()
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

inline static
size_t raw_size(char *s)
{
    size_t i = 0;

    while (s[i]) {
        i++;
    }

    return i;
}

inline static
size_t raw_size_16(uint16_t *s)
{
    size_t i = 0;

    while (s[i]) {
        i++;
    }

    return i;
}

fa_string_utf8_t fa_string_to_utf8(fa_string_t str)
{
    size_t inSize, outSize, cstrSize;
    char *in, *out, *cstr;

    inSize  = str->size * kStandardCodeSize;   // exact char count
    outSize = str->size * 4;            // worst case, we shrink after iconv
    in      = (char *) str->data;
    out     = fa_malloc(outSize);
    cstr    = out;

    {
        iconv_t conv   = iconv_open("UTF-8", kStandardCode);
        size_t  status = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        if (status == ((size_t) - 1)) {
            iconv_fail();
        }
    }

    cstrSize = out - cstr;
    cstr     = fa_realloc(cstr, cstrSize + 1);

    cstr[cstrSize] = 0;                 // add null-terminator

    return cstr;
}

fa_string_cp1252_t fa_string_to_cp1252(fa_string_t str)
{
    size_t inSize, outSize, cstrSize;
    char *in, *out, *cstr;

    inSize  = str->size * kStandardCodeSize;   // exact char count
    outSize = str->size * 4;            // worst case, we shrink after iconv
    in      = (char *) str->data;
    out     = fa_malloc(outSize);
    cstr    = out;

    {
        iconv_t conv   = iconv_open("CP1252", kStandardCode);
        size_t  status = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        if (status == ((size_t) - 1)) {
            iconv_fail();
        }
    }

    cstrSize = out - cstr;
    cstr     = fa_realloc(cstr, cstrSize + 1);

    cstr[cstrSize] = 0;                 // add null-terminator

    return cstr;
}


fa_string_utf16_t fa_string_to_utf16(fa_string_t as)
{
    size_t size = as->size;
    uint16_t *cstr = fa_malloc((size + 1) * kStandardCodeSize);
    memcpy(cstr, as->data, as->size * kStandardCodeSize);
    cstr[size] = 0;
    return cstr;
}

fa_string_t fa_string_from_utf8(fa_string_utf8_t cstr)
{
    size_t inSize, outSize, strSize;
    char *in, *out, *str;

    inSize  = raw_size(cstr);    // char count is in [inSize/4,inSize]
    outSize = inSize * 2;        // worst case, we shrink after iconv
    in      = cstr;
    out     = fa_malloc(outSize);
    str     = out;

    {
        iconv_t conv = iconv_open(kStandardCode, "UTF-8");
        size_t status = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        if (status == ((size_t) - 1)) {
            iconv_fail();
        }
    }

    strSize = out - str;
    str     = fa_realloc(str, strSize);

    return new_string(strSize / kStandardCodeSize, (uint16_t *) str);
}

fa_string_t fa_string_from_cp1252(fa_string_cp1252_t cstr)
{
    size_t inSize, outSize, strSize;
    char *in, *out, *str;

    inSize  = raw_size(cstr);    // char count is in [inSize/4,inSize]
    outSize = inSize * 2;        // worst case, we shrink after iconv
    in      = cstr;
    out     = fa_malloc(outSize);
    str     = out;

    {
        iconv_t conv = iconv_open(kStandardCode, "CP1252");
        size_t status = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        if (status == ((size_t) - 1)) {
            iconv_fail();
        }
    }

    strSize = out - str;
    str     = fa_realloc(str, strSize);

    return new_string(strSize / kStandardCodeSize, (uint16_t *) str);
}

fa_string_t fa_string_from_utf16(fa_string_utf16_t cstr)
{
    size_t size = raw_size_16(cstr);
    string_t as = new_string(size, fa_malloc(size * kStandardCodeSize));
    memcpy(cstr, as->data, as->size * kStandardCodeSize);
    return as;
}


fa_string_t fa_string_show(fa_ptr_t a)
{
    assert(fa_interface(fa_string_show_i, a) && "Must implement Show");
    return ((fa_string_show_t *) fa_interface(fa_string_show_i, a))->show(a);
}

fa_string_t fa_string_to_string(fa_ptr_t a)
{
    assert(fa_interface(fa_string_show_i, a) && "Must implement Show");

    bool is_string = fa_interface(fa_dynamic_i, a)
                     && (fa_dynamic_get_type(a) == string_type_repr);

    if (is_string) {
        return fa_string_copy(a);
    } else {
        return fa_string_show(a);
    }
}

inline static
ptr_t jsonify(ptr_t a)
{
    switch (fa_dynamic_get_type(a)) {
    case pair_type_repr:
        return jsonify(fa_pair_to_list(a));

    case set_type_repr:
        return jsonify(fa_set_to_list(a));

    case list_type_repr:
        return fa_list_map(apply1, jsonify, a);

    case map_type_repr:
        return fa_map_map(apply1, jsonify, a);

    default:
        return a;
    }
}

inline static
ptr_t unjsonify(JSON_Value *a, bool *ok)
{
    switch (json_value_get_type(a)) {
    case JSONError:
        *ok = false;
        return NULL;

    case JSONNull:
        return fa_list_empty();

    case JSONString:
        return string((char *) json_value_get_string(a));

    case JSONNumber:
        return i32(json_value_get_number(a));

    case JSONBoolean:
        return fb(json_value_get_boolean(a));

    case JSONArray: {
        JSON_Array *ar  = json_value_get_array(a);
        size_t sz       = json_array_get_count(ar);
        list_t list     = fa_list_empty();

        for (size_t i = sz; i > 0; --i) {
            ptr_t v = unjsonify(json_array_get_value(ar, i - 1), ok);

            if (!ok) {
                return NULL;
            }

            list = fa_list_dcons(v, list);
        }

        return list;
    }

    case JSONObject: {
        JSON_Object *obj = json_value_get_object(a);
        size_t sz = json_object_get_count(obj);
        map_t map = fa_map_empty();

        for (size_t i = 0; i < sz; ++i) {
            char *name = (char *) json_object_get_name(obj, i);
            ptr_t value = unjsonify(json_object_get_value(obj, name), ok);

            if (!ok) {
                return NULL;
            }

            map = fa_map_dset(string(name), value, map);
        }

        return map;
    }

    default:
        assert(false && "Missing case");
    }
}

fa_string_t fa_string_to_json(fa_ptr_t a)
{
    if (!fa_interface(fa_dynamic_i, a)) {
        return fa_string_show(a);
    } else {
        return fa_string_show(jsonify(a));
    }
}

fa_ptr_t fa_string_from_json(fa_string_t string)
{
    bool ok = true;
    ptr_t result = unjsonify(json_parse_string(unstring(string)), &ok);

    if (!ok) {
        return (fa_ptr_t) string_error(string("Malformed JSON value."));
    } else {
        return result;
    }
}

bool fa_string_matches(fa_string_t expr, fa_string_t string)
{
    if (fa_string_length(expr) <= 0) {
        return false;
    }

    for (int i = 0; i < expr->size; ++i) {
        if (expr->data[i] > 127) {
            assert(false && "Can not handle Unicode chars in expression.");
        }
    }

    char *cexpr    = fa_string_to_utf8(expr);
    char *cinput   = fa_string_to_utf8(string);

    TRex *exp = trex_compile(cexpr, NULL);
    bool res = trex_match(exp, cinput);

    trex_free(exp);
    fa_free(cexpr);
    fa_free(cinput);

    return res;
}


string_t fa_string_join_map(unary_t func, ptr_t data, string_t string)
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
        return fa_string_single(c);
    }
}

inline static string_t escape(string_t string)
{
    return fa_string_join_map(apply1, escape_char, string);
}

static bool string_equal(fa_ptr_t as, fa_ptr_t bs)
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

static bool string_less_than(fa_ptr_t as, fa_ptr_t bs)
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

static bool string_greater_than(fa_ptr_t as, fa_ptr_t bs)
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


static fa_string_t string_show(fa_ptr_t a)
{
    string_t s = string("");
    s = string_dappend(s, string("\""));
    s = string_dappend(s, escape(a));
    s = string_dappend(s, string("\""));
    return s;
}


fa_ptr_t string_copy(fa_ptr_t a)
{
    return fa_string_copy(a);
}

void string_destroy(fa_ptr_t a)
{
    fa_string_destroy(a);
}

type_repr_t string_get_type(fa_ptr_t a)
{
    return string_type_repr;
}

fa_ptr_t string_impl(fa_id_t interface)
{
    static fa_equal_t string_equal_impl = { string_equal };
    static fa_copy_t string_copy_impl = { string_copy };
    static fa_string_show_t string_show_impl = { string_show };
    static fa_destroy_t string_destroy_impl = { string_destroy };
    static fa_order_t string_order_impl = { string_less_than, string_greater_than };
    static fa_dynamic_t string_dynamic_impl = { string_get_type };

    switch (interface) {
    case fa_equal_i:
        return &string_equal_impl;

    case fa_order_i:
        return &string_order_impl;

    case fa_string_show_i:
        return &string_show_impl;

    case fa_copy_i:
        return &string_copy_impl;

    case fa_destroy_i:
        return &string_destroy_impl;

    case fa_dynamic_i:
        return &string_dynamic_impl;

    default:
        return NULL;
    }
}

error_t string_error(string_t msg)
{
    return fa_error_create_simple(error,
                                  msg,
                                  string("Fa.String"));
}

void string_fatal(char *msg, int error)
{
    void fa_fa_log_error_from(fa_string_t msg, fa_string_t origin);

    fa_fa_log_error_from(string(msg), string("Doremir.String"));
    exit(error);
}

