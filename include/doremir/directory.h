
#ifndef _DOREMIR_DIRECTORY
#define _DOREMIR_DIRECTORY

#include <doremir.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDirectory Directory
    @{
    */

doremir_string_file_path_t doremir_directory_home();
doremir_string_file_path_t doremir_directory_current();
doremir_string_t doremir_directory_read_file(doremir_string_file_path_t);
void doremir_directory_write_file(doremir_string_file_path_t,
                                  doremir_string_t);
void doremir_directory_append_file(doremir_string_file_path_t,
                                   doremir_string_t);

/** @}
    @}
    */

#endif // _DOREMIR_DIRECTORY

