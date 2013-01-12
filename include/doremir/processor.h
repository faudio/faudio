
#ifndef _DOREMIR_PROCESSOR
#define _DOREMIR_PROCESSOR

#include <doremir.h>
#include <doremir/time.h>
#include <doremir/type.h>
#include <doremir/buffer.h>
#include <doremir/dispatcher.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    */

typedef struct {
            double sample_rate;
            size_t num_samples;
            size_t sample_time;
            doremir_time_t total_time;
            doremir_dispatcher_t dispatcher;
        } doremir_processor_info_t;
typedef doremir_buffer_t doremir_processor_samples_t;
typedef struct {
            void (* before)(doremir_ptr_t, doremir_processor_info_t *);
            void (* process)(doremir_ptr_t,
                             doremir_processor_info_t *,
                             doremir_processor_samples_t);
            void (* after)(doremir_ptr_t, doremir_processor_info_t *);
            doremir_type_t (* input_type)(doremir_ptr_t);
            doremir_type_t (* output_type)(doremir_ptr_t);
        } doremir_processor_interface_t;
typedef struct _doremir_processor_t * doremir_processor_t;
doremir_processor_t doremir_processor_unary(doremir_type_t,
                                            doremir_type_t,
                                            doremir_unary_t);
doremir_processor_t doremir_processor_binary(doremir_type_t,
                                             doremir_type_t,
                                             doremir_type_t,
                                             doremir_binary_t);
doremir_processor_t doremir_processor_ternary(doremir_type_t,
                                              doremir_type_t,
                                              doremir_type_t,
                                              doremir_type_t,
                                              doremir_ternary_t);
doremir_processor_t doremir_processor_identity(doremir_type_t);
doremir_processor_t doremir_processor_constant(doremir_type_t,
                                               doremir_type_t,
                                               doremir_ptr_t);
doremir_processor_t doremir_processor_delay(doremir_type_t,
                                            size_t);
doremir_processor_t doremir_processor_split(doremir_type_t);
doremir_processor_t doremir_processor_seq(doremir_processor_t,
                                          doremir_processor_t);
doremir_processor_t doremir_processor_par(doremir_processor_t,
                                          doremir_processor_t);
doremir_processor_t doremir_processor_loop(doremir_processor_t);

/** @}
    @}
    */

#endif // _DOREMIR_PROCESSOR

