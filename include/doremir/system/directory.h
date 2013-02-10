
#ifndef _DOREMIR_SYSTEM_DIRECTORY
#define _DOREMIR_SYSTEM_DIRECTORY

#include <doremir.h>
#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirSystem System
    @{
    @defgroup DoremirSystemDirectory Directory
    @{
    */

doremir_string_file_path_t doremir_system_directory_home();
doremir_string_file_path_t doremir_system_directory_current();
void doremir_system_directory_create(doremir_string_file_path_t);
void doremir_system_directory_remove(doremir_string_file_path_t);
doremir_string_t doremir_system_directory_read_file(doremir_string_file_path_t);
void doremir_system_directory_write_file(doremir_string_file_path_t,
                                         doremir_string_t);
void doremir_system_directory_append_file(doremir_string_file_path_t,
                                          doremir_string_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_SYSTEM_DIRECTORY

