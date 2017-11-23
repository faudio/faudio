
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/string.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <iconv.h>

#include <CoreFoundation/CoreFoundation.h>

void *fa_string_to_native(fa_string_t str)
{
    const char *cstr    = fa_string_peek_utf8(str);
    CFStringRef cfstr   = CFStringCreateWithCString(kCFAllocatorDefault, cstr, kCFStringEncodingUTF8);

    return (void *) cfstr;
}

fa_string_t fa_string_from_native(void *input)
{
    CFStringRef cfRef = input;
    CFIndex size;
    char *cstr;
    fa_string_t str;

    if ((cstr = (char *) CFStringGetCStringPtr(cfRef, kCFStringEncodingUTF8))) {
        return fa_string_from_utf8(cstr);
    } else {
        size        = CFStringGetLength(cfRef);
        cstr        = fa_malloc(size + 1);
        cstr[size]  = 0;

        CFStringGetCString(cfRef, cstr, size + 1, kCFStringEncodingUTF8);
        str = fa_string_from_utf8(cstr);

        fa_free(cstr);
        return str;
    }
}
