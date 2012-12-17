
#include <doremir/string.h>
#include <doremir/util.h>
#include <iconv.h>

struct _doremir_string_t
{               
    size_t  size;
    int16_t *data; // UTF-16
};
static const size_t kCharSize = sizeof(int16_t);


doremir_string_t doremir_string_empty()
{
    doremir_string_t str = doremir_new(string);
    str->size = 0;
    return str;
}

doremir_string_t doremir_string_single(uint16_t chr)
{
    doremir_string_t str = doremir_new(string);
    str->size = 1;
    str->data = malloc(kCharSize);
    str->data[0] = chr;
    return str;
}

doremir_string_t doremir_string_copy(doremir_string_t str)
{
    doremir_string_t pst = doremir_new(string);
    pst->size = str->size;
    pst->data = malloc(str->size * kCharSize);
    memcpy(pst->data, str->data, str->size * kCharSize);
    return pst;
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

doremir_string_utf8_t doremir_string_to_utf8(doremir_string_t str)
{   
    size_t inSize  = str->size * kCharSize;
    size_t bufSize = inSize * 3 + 1;
    size_t outSize = bufSize;
    char* out = malloc(outSize);
    char* buf = out;
    {
        iconv_t conv = iconv_open("UTF-8", "UTF-16BE");
        iconv(conv, &str->data, &inSize, &buf, &outSize);
        iconv_close(conv);
    }
    size_t size = bufSize - outSize + 1; // written bytes + null-terminator
    out = realloc(out, size);

    // TODO error handling            
    return out;
}

doremir_string_utf16_t doremir_string_to_utf16(doremir_string_t str)
{
    assert(false && "Not implemented");
}

doremir_string_utf32_t doremir_string_to_utf32(doremir_string_t str)
{
    assert(false && "Not implemented");
}

static size_t Utf8Size(char *s) 
{
   size_t i = 0, j = 0;
   while (s[i]) {
     if ((s[i] & 0xc0) != 0x80) j++;
     i++;
   }
   return j + 1;
}

doremir_string_t doremir_string_from_utf8(doremir_string_utf8_t cstr)
{
    size_t inSize  = Utf8Size(cstr);
    printf(">>>inSize: %d\n", inSize);
    size_t outSize = inSize * 3 + 1;
    char* buf = malloc(outSize);
    char* out = buf;
    {
        iconv_t conv = iconv_open("UTF-16BE", "UTF-8");
        iconv(conv, &cstr, &inSize, &out, &outSize);
        iconv_close(conv);
    }
    size_t size = out - buf; // written bytes
    printf(">>>: %d\n", size);
    buf = realloc(buf, size);
    
    // TODO error handling            
    doremir_string_t pst = doremir_new(string);
    pst->size = size / 2; // number of chars
    pst->data = buf;
    return pst;
}

doremir_string_t doremir_string_from_utf16(doremir_string_utf16_t cstr)
{
    assert(false && "Not implemented");
}

doremir_string_t doremir_string_from_utf32(doremir_string_utf32_t cstr)
{
    assert(false && "Not implemented");
}

