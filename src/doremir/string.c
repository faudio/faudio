
#include <doremir/string.h>
#include <doremir/util.h>
#include <iconv.h>

doremir_ptr_t string_impl(doremir_id_t interface);

struct _doremir_string_t
{               
    doremir_impl_t  impl;
    size_t          size;
    uint16_t        *data;
};

// TODO Should be based on the platform endianness
#define kStdCode  "UTF-16LE" 
#define kCharSize sizeof(uint16_t)


doremir_string_t new_string(size_t size, uint16_t *data)
{
    doremir_string_t str = doremir_new(string);
    str->impl = &string_impl;    
    str->size = size;
    str->data = data;
    return str;
}

// --------------------------------------------------------------------------------

doremir_string_t doremir_string_empty()
{
    return new_string(0, NULL);
}

doremir_string_t doremir_string_single(uint16_t chr)
{
    doremir_string_t str = new_string(1, NULL);

    str->data = malloc(kCharSize);
    str->data[0] = chr;

    return str;
}

doremir_string_t doremir_string_copy(doremir_string_t str)
{
    doremir_string_t pst = new_string(str->size, NULL);

    pst->data = malloc(str->size * kCharSize);
    memcpy(pst->data, str->data, str->size * kCharSize);

    return pst;
}

doremir_string_t doremir_string_append(doremir_string_t as,
                                       doremir_string_t bs)
{
    doremir_string_t cs = new_string(as->size + bs->size, NULL);
    cs->data = malloc(cs->size*kCharSize);
    memcpy(cs->data, as->data, as->size*kCharSize);
    memcpy(cs->data + as->size, bs->data, bs->size*kCharSize);
    return cs;
}

doremir_string_t doremir_string_dappend(doremir_string_t as,
                                       doremir_string_t bs)
{
    doremir_string_t cs = new_string(as->size + bs->size, NULL);
    cs->data = realloc(as->data, cs->size*kCharSize);
    memcpy(cs->data + as->size, bs->data, bs->size*kCharSize);
    doremir_delete(as);
    return cs;
}

void doremir_string_destroy(doremir_string_t str)
{                     
    free(str->data);
    doremir_delete(str);
}

// --------------------------------------------------------------------------------
// Predicates etc
// --------------------------------------------------------------------------------

int doremir_string_length(doremir_string_t str)
{
    return str->size;
}     

uint16_t doremir_string_char_at(int n, doremir_string_t str)
{                       
    if (n < 0 || n >= str->size)
        assert(false && "Out of range");
    return str->data[n];
}

// --------------------------------------------------------------------------------
// Conversion functions
// --------------------------------------------------------------------------------

doremir_string_utf8_t doremir_string_to_utf8(doremir_string_t str)
{   
    size_t inSize  = str->size * kCharSize;
    size_t outSize = inSize * 3 / 2; // worst case, we resize after iconv

    char* buf = malloc(outSize);
    char* in  = (char*) str->data;
    char* out = buf;
    {
        iconv_t conv = iconv_open("UTF-8", kStdCode);
        size_t res = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        // TODO fatal
        if (res < 0)
        {
            switch (errno)
            {
                case E2BIG:  assert(false);
                case EILSEQ: assert(false);
                case EINVAL: assert(false);
                default:     assert(false);
            }
        }
    }
    size_t size = out - buf;
    buf = realloc(buf, size + 1);
    buf[size] = 0; // null-terminate
    return buf;
}

doremir_string_utf16_t doremir_string_to_utf16(doremir_string_t as)
{
    size_t size = as->size;
    uint16_t* cstr = malloc((size + 1)*kCharSize);
    memcpy(cstr, as->data, as->size*kCharSize);
    cstr[size] = 0;
    return cstr;
}

doremir_string_utf32_t doremir_string_to_utf32(doremir_string_t str)
{
    assert(false && "Not implemented");
}

// static size_t Utf8Size(char *s) 
// {
//    size_t i = 0, j = 0;
//    while (s[i]) {
//      if ((s[i] & 0xc0) != 0x80) j++;
//      i++;
//    }
//    return j;
// }
static size_t RawSize(char *s) 
{
   size_t i = 0;
   while (s[i])
     i++;
   return i;
}     
static size_t RawSize16(uint16_t *s) 
{
   size_t i = 0;
   while (s[i])
     i++;
   return i;
}     
// static size_t RawSize32(uint32_t *s) 
// {
//    size_t i = 0;
//    while (s[i])
//      i++;
//    return i;
// }     

doremir_string_t doremir_string_from_utf8(doremir_string_utf8_t cstr)
{
    size_t inSize  = RawSize(cstr);
    size_t outSize = inSize * 3; // worst case, we resize after iconv

    char* buf = malloc(outSize);
    char* in  = cstr;
    char* out = buf;
    {
        iconv_t conv = iconv_open(kStdCode, "UTF-8");
        size_t res = iconv(conv, &in, &inSize, &out, &outSize);
        iconv_close(conv);

        // TODO fatal
        if (res < 0)
        {
            switch (errno)
            {
                case E2BIG:  assert(false);
                case EILSEQ: assert(false);
                case EINVAL: assert(false);
                default:     assert(false);
            }
        }

    }
    size_t size = out - buf; // written bytes
    buf = realloc(buf, size);
    
    doremir_string_t pst = new_string(size / 2, NULL);
    pst->data = (uint16_t*) buf;
    return pst;
}

doremir_string_t doremir_string_from_utf16(doremir_string_utf16_t cstr)
{
    size_t size = RawSize16(cstr);

    doremir_string_t as = new_string(size, NULL);
    as->data = malloc(size*kCharSize);

    memcpy(cstr, as->data, as->size*kCharSize);
    return as;
}

doremir_string_t doremir_string_from_utf32(doremir_string_utf32_t cstr)
{
    assert(false && "Not implemented");
}   

// --------------------------------------------------------------------------------

doremir_string_t doremir_string_show(doremir_ptr_t a)
{
    return ((doremir_string_show_t*) doremir_interface(doremir_string_show_i, a))->show(a);
}     

// --------------------------------------------------------------------------------

bool string_equal(doremir_ptr_t as, doremir_ptr_t bs)
{                                                   
    doremir_string_t cs = (doremir_string_t) as;
    doremir_string_t ds = (doremir_string_t) bs;
    
    for (int i = 0; i < cs->size && i < ds->size; ++i)
    {
        if (cs->data[i] != ds->data[i])
            return false;
    }
    return true;
}

bool string_less_than(doremir_ptr_t as, doremir_ptr_t bs)
{
    doremir_string_t cs = (doremir_string_t) as;
    doremir_string_t ds = (doremir_string_t) bs;
    
    for (int i = 0; i < cs->size && i < ds->size; ++i)
    {
        if (cs->data[i] < ds->data[i])
            return true;
        if (cs->data[i] > ds->data[i])
            return false;
    }
    return false;
}

bool string_greater_than(doremir_ptr_t as, doremir_ptr_t bs)
{
    doremir_string_t cs = (doremir_string_t) as;
    doremir_string_t ds = (doremir_string_t) bs;
    
    for (int i = 0; i < cs->size && i < ds->size; ++i)
    {
        if (cs->data[i] > ds->data[i])
            return true;
        if (cs->data[i] < ds->data[i])
            return false;
    }
    return false;
}

doremir_string_t string_show(doremir_ptr_t a)
{               
    return doremir_string_copy(a);    
}

doremir_ptr_t string_copy(doremir_ptr_t a)
{
    return doremir_string_copy(a);
}

void string_destroy(doremir_ptr_t a)
{
    doremir_string_destroy(a);
}

doremir_ptr_t string_impl(doremir_id_t interface)
{
    static doremir_equal_t string_equal_impl = { string_equal };
    static doremir_copy_t string_copy_impl = { string_copy };
    static doremir_string_show_t string_show_impl = { string_show };
    static doremir_destroy_t string_destroy_impl = { string_destroy };
    static doremir_order_t string_order_impl = { string_less_than, string_greater_than };

    switch (interface)
    {
    case doremir_equal_i:
        return &string_equal_impl;

    case doremir_order_i:
        return &string_order_impl;

    case doremir_string_show_i:
        return &string_show_impl;

    case doremir_copy_i:
        return &string_copy_impl;

    case doremir_destroy_i:
        return &string_destroy_impl;

    default:
        return NULL;
    }
}

