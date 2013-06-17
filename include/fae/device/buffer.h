
#ifndef _FAE_DEVICE_BUFFER
#define _FAE_DEVICE_BUFFER

#include <fae/device.h>
#include <fae/processor.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeDevice Device
    @{
    @defgroup FaeDeviceBuffer Buffer
    @{
    */

typedef struct _fae_device_buffer_result_t * fae_device_buffer_result_t;
fae_device_buffer_t fae_device_buffer_open(size_t);
void fae_device_buffer_close(fae_device_buffer_t);
fae_device_buffer_result_t fae_device_buffer_run(fae_device_buffer_t,
                                                 fae_processor_t,
                                                 fae_device_buffer_t);

/** @}
    @}
    @}
    */

#endif // _FAE_DEVICE_BUFFER

