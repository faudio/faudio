
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2016
    All rights reserved.

 */

#include <fa/string.h>

struct _fa_string_t {
    fa_impl_t       impl;           // Dispatcher
    size_t          size;           // Byte (NOT character) count
    uint8_t         *data;          // Payload
};

#include <wchar.h>
#include <windows.h>

wchar_t* fa_string_to_wstr(fa_string_t str)
{
    int wlength = MultiByteToWideChar(CP_UTF8, 0, str->data, str->size, 0, 0);
    LPWSTR wstr = (LPWSTR)calloc(wlength+1, sizeof(wchar_t));
    MultiByteToWideChar(CP_UTF8, 0, str->data, str->size, wstr, wlength);
    return wstr;
}
