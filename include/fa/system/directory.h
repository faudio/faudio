
#ifndef _FA_SYSTEM_DIRECTORY
#define _FA_SYSTEM_DIRECTORY

#include <fa.h>
#include <fa/string.h>

/** @addtogroup FaSystemDirectory

    Cross-platform directory utilities.    
 
    @defgroup Fa Fa
    @{
    @defgroup FaSystem System
    @{
    @defgroup FaSystemDirectory Directory
    @{
    */


fa_string_t fa_system_directory_home();


fa_string_t fa_system_directory_current();


void fa_system_directory_create(fa_string_t);


void fa_system_directory_remove(fa_string_t);


fa_string_t fa_system_directory_read_file(fa_string_t);


void fa_system_directory_write_file(fa_string_t, fa_string_t);


void fa_system_directory_append_file(fa_string_t, fa_string_t);

/** @}
    @}
    @}
    */

#endif // _FA_SYSTEM_DIRECTORY

