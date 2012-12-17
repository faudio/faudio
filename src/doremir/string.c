
#include <doremir/string.h>
#include <doremir/util.h>
#include <iconv.h>

struct _doremir_string_t
{               
    size_t   size;
    uint16_t *data;
};
#define kCharSize sizeof(uint16_t)
#define kStdCode  "UTF-16LE" // TODO match to endianness of platform

static void memdump(void* s, size_t n)
{        
    for (size_t i = 0; i < n; ++i)
        printf("%02x ", *((unsigned char*) s + i) );
    printf("\n");
}



doremir_string_t NewString(size_t size, uint16_t *data)
{
    doremir_string_t str = doremir_new(string);
    str->size = size;
    str->data = data;
    return str;
}

doremir_string_t doremir_string_empty()
{
    return NewString(0, NULL);
}

doremir_string_t doremir_string_single(uint16_t chr)
{
    doremir_string_t str = NewString(1, NULL);

    str->data = malloc(kCharSize);
    str->data[0] = chr;

    return str;
}

doremir_string_t doremir_string_copy(doremir_string_t str)
{
    doremir_string_t pst = NewString(str->size, NULL);

    pst->data = malloc(str->size * kCharSize);
    memcpy(pst->data, str->data, str->size * kCharSize);

    return pst;
}

doremir_string_t doremir_string_append(doremir_string_t as,
                                       doremir_string_t bs)
{
    doremir_string_t cs = NewString(as->size + bs->size, NULL);
    cs->data = malloc(cs->size*kCharSize);
    memcpy(cs->data, as->data, as->size*kCharSize);
    memcpy(cs->data + as->size, bs->data, bs->size*kCharSize);
    return cs;
}

doremir_string_t doremir_string_dappend(doremir_string_t as,
                                       doremir_string_t bs)
{
    doremir_string_t cs = NewString(as->size + bs->size, NULL);
    cs->data = realloc(as->data, cs->size*kCharSize);
    memcpy(cs->data + as->size, bs->data, bs->size*kCharSize);
    return cs;
}

void doremir_string_destroy(doremir_string_t str)
{                     
    free(str->data);
    doremir_delete(str);
}

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

doremir_string_utf8_t doremir_string_to_utf8(doremir_string_t str)
{   
    size_t inSize  = str->size * kCharSize;
    size_t outSize = inSize * 3 / 2; // worst case, we resize after iconv

    char* buf = malloc(outSize);
    char* in  = str->data;
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

doremir_string_utf16_t doremir_string_to_utf16(doremir_string_t str)
{
    assert(false && "Not implemented");
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
    
    doremir_string_t pst = NewString(size / 2, NULL);
    pst->data = (uint16_t*) buf;
    return pst;
}

doremir_string_t doremir_string_from_utf16(doremir_string_utf16_t cstr)
{
    // TODO normalize byte order to BE
    assert(false && "Not implemented");
}

doremir_string_t doremir_string_from_utf32(doremir_string_utf32_t cstr)
{
    assert(false && "Not implemented");
}

