
#ifndef _DOREMIR_PROCESSOR
#define _DOREMIR_PROCESSOR

#include <doremir.h>
#include <doremir/time.h>
#include <doremir/type.h>
#include <doremir/buffer.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirProcessor Processor
    @{
    */

typedef struct {
            double sample_rate;
            size_t sample_count;
            size_t vector_size;
            doremir_time_t real_time;
        } doremir_processor_info_t;
typedef doremir_buffer_t doremir_processor_samples_t;
typedef struct {
            void (* before)(doremir_ptr_t, doremir_processor_info_t *);
            doremir_processor_samples_t (* process)(doremir_ptr_t,
                                                    doremir_processor_info_t *,
                                                    doremir_processor_samples_t);
            void (* after)(doremir_ptr_t, doremir_processor_info_t *);
        } doremir_processor_t;
typedef doremir_ptr_t doremir_processor_any_t;
doremir_processor_any_t doremir_processor_lift(doremir_type_t,
                                               doremir_unary_t *);
doremir_processor_any_t doremir_processor_lift2(doremir_type_t,
                                                doremir_binary_t *);
doremir_processor_any_t doremir_processor_lift3(doremir_type_t,
                                                doremir_ternary_t *);
doremir_processor_any_t doremir_processor_id(doremir_type_t);
doremir_processor_any_t doremir_processor_const(doremir_type_t,
                                                doremir_ptr_t);
doremir_processor_any_t doremir_processor_delay(doremir_type_t,
                                                size_t);
doremir_processor_any_t doremir_processor_split(doremir_type_t);
doremir_processor_any_t doremir_processor_seq(doremir_processor_any_t,
                                              doremir_processor_any_t);
doremir_processor_any_t doremir_processor_par(doremir_processor_any_t,
                                              doremir_processor_any_t);
doremir_processor_any_t doremir_processor_loop(doremir_processor_any_t);
doremir_processor_any_t doremir_processor_cos(doremir_type_t);
doremir_processor_any_t doremir_processor_sin(doremir_type_t);
doremir_processor_any_t doremir_processor_tan(doremir_type_t);
doremir_processor_any_t doremir_processor_acos(doremir_type_t);
doremir_processor_any_t doremir_processor_asin(doremir_type_t);
doremir_processor_any_t doremir_processor_atan(doremir_type_t);
doremir_processor_any_t doremir_processor_add(doremir_type_t);
doremir_processor_any_t doremir_processor_subtract(doremir_type_t);
doremir_processor_any_t doremir_processor_multiply(doremir_type_t);
doremir_processor_any_t doremir_processor_divide(doremir_type_t);
doremir_processor_any_t doremir_processor_modulo(doremir_type_t);
doremir_processor_any_t doremir_processor_absolute(doremir_type_t);
doremir_processor_any_t doremir_processor_and(doremir_type_t);
doremir_processor_any_t doremir_processor_or(doremir_type_t);
doremir_processor_any_t doremir_processor_not(doremir_type_t);
doremir_processor_any_t doremir_processor_bit_and(doremir_type_t);
doremir_processor_any_t doremir_processor_bit_or(doremir_type_t);
doremir_processor_any_t doremir_processor_bit_not(doremir_type_t);
doremir_processor_any_t doremir_processor_bit_xor(doremir_type_t);

/** @}
    @}
    */

#endif // _DOREMIR_PROCESSOR

