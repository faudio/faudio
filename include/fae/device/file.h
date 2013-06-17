
#ifndef _FAE_DEVICE_FILE
#define _FAE_DEVICE_FILE

#include <fae/string.h>
#include <fae/device.h>
#include <fae/processor.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeDevice Device
    @{
    @defgroup FaeDeviceFile File
    @{
    */

typedef struct _fae_device_file_result_t * fae_device_file_result_t;
fae_device_file_t fae_device_file_open(fae_string_file_path_t);
void fae_device_file_close(fae_device_file_t);
fae_device_file_result_t fae_device_file_run(fae_device_file_t,
                                             fae_processor_t,
                                             fae_device_file_t);

/** @}
    @}
    @}
    */

#endif // _FAE_DEVICE_FILE

