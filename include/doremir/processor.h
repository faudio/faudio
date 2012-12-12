
#ifndef _DOREMIR_PROCESSOR
#define _DOREMIR_PROCESSOR

#include <doremir.h>
#include <doremir/buffer.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    */

typedef doremir_buffer_t doremir_processor_samples_t;
typedef struct {
            double sample_rate; long sample_count; bool real_time;
        } doremir_processor_info_t;
typedef struct {
            void (* before)(doremir_ptr_t, doremir_processor_info_t *);
            doremir_processor_samples_t (* process)(doremir_ptr_t,
                                                    doremir_processor_info_t *,
                                                    doremir_processor_samples_t);
            void (* after)(doremir_ptr_t, doremir_processor_info_t *);
        } doremir_processor_t;

/** @}
    @}
    */

#endif // _DOREMIR_PROCESSOR

