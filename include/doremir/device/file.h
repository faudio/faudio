
#ifndef _DOREMIR_DEVICE_FILE
#define _DOREMIR_DEVICE_FILE

#include <doremir/string.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDevice Device
    @{
    @defgroup DoremirDeviceFile File
    @{
    */

typedef struct _doremir_device_file_t * doremir_device_file_t;
typedef struct _doremir_device_file_stream_t * doremir_device_file_stream_t;
doremir_device_file_t doremir_device_file_create(doremir_string_file_path_t,
                                                 doremir_string_file_path_t);
void doremir_device_file_destroy(doremir_device_file_t);
void doremir_device_file_close_stream(doremir_device_file_stream_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_DEVICE_FILE

