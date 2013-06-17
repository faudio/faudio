
#ifndef _FAE_STRING_LIST
#define _FAE_STRING_LIST

#include <fae/string.h>
#include <fae/list.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeString String
    @{
    @defgroup FaeStringList List
    @{
    */

fae_list_t fae_string_list_convert(fae_string_t);
fae_string_t fae_string_list_unconvert(fae_list_t);

/** @}
    @}
    @}
    */

#endif // _FAE_STRING_LIST

