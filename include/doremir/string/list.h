
#ifndef _DOREMIR_STRING_LIST
#define _DOREMIR_STRING_LIST

#include <doremir/string.h>
#include <doremir/list.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirString String
    @{
    @defgroup DoremirStringList List
    @{
    */

doremir_list_t doremir_string_list_convert(doremir_string_t);
doremir_string_t doremir_string_list_unconvert(doremir_list_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_STRING_LIST

