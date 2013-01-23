
#ifndef _DOREMIR_PROCESSOR
#define _DOREMIR_PROCESSOR

#include <doremir.h>
#include <doremir/time.h>
#include <doremir/type.h>
#include <doremir/buffer.h>
#include <doremir/message.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    */

typedef struct {
            double sample_rate;
            size_t frame_size;
            size_t sample_time;
            doremir_time_t total_time;
            doremir_message_dispatcher_t dispatcher;
        } doremir_processor_info_t;
typedef doremir_ptr_t doremir_processor_samples_t;
typedef struct {
            void (* before)(doremir_ptr_t, doremir_processor_info_t *);
            void (* process)(doremir_ptr_t,
                             doremir_processor_info_t *,
                             doremir_processor_samples_t);
            void (* after)(doremir_ptr_t, doremir_processor_info_t *);
            doremir_type_t (* input_type)(doremir_ptr_t);
            doremir_type_t (* output_type)(doremir_ptr_t);
            size_t (* buffer_size)(doremir_ptr_t);
        } doremir_processor_interface_t;
typedef struct _doremir_processor_t * doremir_processor_t;
doremir_type_t doremir_processor_input_type(doremir_processor_t);
doremir_type_t doremir_processor_output_type(doremir_processor_t);
doremir_processor_t doremir_processor_identity(doremir_type_t);
doremir_processor_t doremir_processor_constant(doremir_type_t,
                                               doremir_type_t,
                                               doremir_ptr_t);
doremir_processor_t doremir_processor_split(doremir_type_t);
doremir_processor_t doremir_processor_par(doremir_processor_t,
                                          doremir_processor_t);
doremir_processor_t doremir_processor_seq(doremir_processor_t,
                                          doremir_processor_t);
doremir_processor_t doremir_processor_compose(doremir_processor_t,
                                              doremir_processor_t);
doremir_processor_t doremir_processor_loop(doremir_processor_t);
doremir_processor_t doremir_processor_unary(doremir_type_t,
                                            doremir_type_t,
                                            doremir_unary_t,
                                            doremir_ptr_t);
doremir_processor_t doremir_processor_binary(doremir_type_t,
                                             doremir_type_t,
                                             doremir_type_t,
                                             doremir_binary_t,
                                             doremir_ptr_t);
doremir_processor_t doremir_processor_delay(doremir_type_t,
                                            size_t);
doremir_processor_t doremir_processor_add(doremir_type_t);
doremir_processor_t doremir_processor_subtract(doremir_type_t);
doremir_processor_t doremir_processor_multiply(doremir_type_t);
doremir_processor_t doremir_processor_power(doremir_type_t);
doremir_processor_t doremir_processor_divide(doremir_type_t);
doremir_processor_t doremir_processor_modulo(doremir_type_t);
doremir_processor_t doremir_processor_absolute(doremir_type_t);
doremir_processor_t doremir_processor_not(doremir_type_t);
doremir_processor_t doremir_processor_and(doremir_type_t);
doremir_processor_t doremir_processor_or(doremir_type_t);
doremir_processor_t doremir_processor_xor(doremir_type_t);
doremir_processor_t doremir_processor_bit_not(doremir_type_t);
doremir_processor_t doremir_processor_bit_and(doremir_type_t);
doremir_processor_t doremir_processor_bit_or(doremir_type_t);
doremir_processor_t doremir_processor_bit_xor(doremir_type_t);
doremir_processor_t doremir_processor_shift_left(doremir_type_t);
doremir_processor_t doremir_processor_shift_right(doremir_type_t);
doremir_processor_t doremir_processor_equal(doremir_type_t);
doremir_processor_t doremir_processor_less_than(doremir_type_t);
doremir_processor_t doremir_processor_greater_than(doremir_type_t);
doremir_processor_t doremir_processor_less_than_equal(doremir_type_t);
doremir_processor_t doremir_processor_greater_than_equal(doremir_type_t);
doremir_processor_t doremir_processor_acos(doremir_type_t);
doremir_processor_t doremir_processor_asin(doremir_type_t);
doremir_processor_t doremir_processor_atan(doremir_type_t);
doremir_processor_t doremir_processor_cos(doremir_type_t);
doremir_processor_t doremir_processor_sin(doremir_type_t);
doremir_processor_t doremir_processor_tan(doremir_type_t);
doremir_processor_t doremir_processor_exp(doremir_type_t);
doremir_processor_t doremir_processor_log(doremir_type_t);
doremir_processor_t doremir_processor_log10(doremir_type_t);
doremir_processor_t doremir_processor_pow(doremir_type_t);
doremir_processor_t doremir_processor_sqrt(doremir_type_t);
doremir_processor_t doremir_processor_abs(doremir_type_t);
doremir_processor_t doremir_processor_min(doremir_type_t);
doremir_processor_t doremir_processor_max(doremir_type_t);
doremir_processor_t doremir_processor_fmod(doremir_type_t);
doremir_processor_t doremir_processor_remainder(doremir_type_t);
doremir_processor_t doremir_processor_floor(doremir_type_t);
doremir_processor_t doremir_processor_ceil(doremir_type_t);
doremir_processor_t doremir_processor_rint(doremir_type_t);

/** @}
    @}
    */

#endif // _DOREMIR_PROCESSOR

