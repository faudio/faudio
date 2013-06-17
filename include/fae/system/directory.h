
#ifndef _FAE_SYSTEM_DIRECTORY
#define _FAE_SYSTEM_DIRECTORY

#include <fae.h>
#include <fae/string.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeSystem System
    @{
    @defgroup FaeSystemDirectory Directory
    @{
    */

typedef fae_string_t fae_system_directory_file_path_t;
fae_system_directory_file_path_t fae_system_directory_home();
fae_system_directory_file_path_t fae_system_directory_current();
void fae_system_directory_create(fae_system_directory_file_path_t);
void fae_system_directory_remove(fae_system_directory_file_path_t);
fae_string_t fae_system_directory_read_file(fae_system_directory_file_path_t);
void fae_system_directory_write_file(fae_system_directory_file_path_t,
                                     fae_string_t);
void fae_system_directory_append_file(fae_system_directory_file_path_t,
                                      fae_string_t);

/** @}
    @}
    @}
    */

#endif // _FAE_SYSTEM_DIRECTORY

