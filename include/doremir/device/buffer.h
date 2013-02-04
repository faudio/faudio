
#ifndef _DOREMIR_DEVICE_BUFFER
#define _DOREMIR_DEVICE_BUFFER

#include <doremir/device.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirDevice Device
    @{
    @defgroup DoremirDeviceBuffer Buffer
    @{
    */

typedef struct _doremir_device_buffer_result_t * doremir_device_buffer_result_t;
doremir_device_buffer_t doremir_device_buffer_open(size_t);
void doremir_device_buffer_close(doremir_device_buffer_t);
doremir_device_buffer_result_t doremir_device_buffer_run(doremir_device_buffer_t,
                                                         doremir_processor_t,
                                                         doremir_device_buffer_t);

/** @}
    @}
    @}
    */

#endif // _DOREMIR_DEVICE_BUFFER

