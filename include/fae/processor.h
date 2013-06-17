
#ifndef _FAE_PROCESSOR
#define _FAE_PROCESSOR

#include <fae.h>
#include <fae/time.h>
#include <fae/type.h>
#include <fae/graph.h>
#include <fae/buffer.h>
#include <fae/message.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeProcessor Processor
    @{
    */

typedef struct {
            double sample_rate;
            size_t frame_size;
            size_t sample_time;
            fae_time_t total_time;
            fae_message_dispatcher_t dispatcher;
            int buf_offset;
            int buf_step;
            int buf_loop;
            int buf_seq;
        } fae_processor_info_t;
typedef fae_ptr_t * fae_processor_samples_t;
typedef struct {
            void (* before)(fae_ptr_t, fae_processor_info_t *);
            void (* process)(fae_ptr_t,
                             fae_processor_info_t *,
                             fae_processor_samples_t);
            void (* after)(fae_ptr_t, fae_processor_info_t *);
            fae_type_t (* input_type)(fae_ptr_t);
            fae_type_t (* output_type)(fae_ptr_t);
            size_t (* buffer_size)(fae_type_frames_t, fae_ptr_t);
            fae_graph_t (* graph)(fae_ptr_t,
                                  fae_processor_info_t *,
                                  fae_graph_t);
        } fae_processor_interface_t;
typedef struct _fae_processor_t * fae_processor_t;
fae_type_t fae_processor_input_type(fae_processor_t);
fae_type_t fae_processor_output_type(fae_processor_t);
size_t fae_processor_buffer_size(fae_type_frames_t,
                                 fae_processor_t);
fae_ptr_t fae_processor_address(fae_processor_t);
fae_graph_t fae_processor_graph(fae_processor_t,
                                fae_processor_info_t *,
                                fae_graph_t);
void fae_processor_write_graph(fae_processor_t,
                               fae_string_file_path_t);
fae_processor_t fae_processor_identity(fae_type_t);
fae_processor_t fae_processor_constant(fae_type_t,
                                       fae_type_t,
                                       fae_ptr_t);
fae_processor_t fae_processor_delay(fae_type_t, size_t);
fae_processor_t fae_processor_split(fae_type_t);
fae_processor_t fae_processor_unary(fae_type_t,
                                    fae_type_t,
                                    fae_unary_t,
                                    fae_ptr_t);
fae_processor_t fae_processor_binary(fae_type_t,
                                     fae_type_t,
                                     fae_type_t,
                                     fae_binary_t,
                                     fae_ptr_t);
fae_processor_t fae_processor_parallel(fae_processor_t,
                                       fae_processor_t);
fae_processor_t fae_processor_sequence(fae_processor_t,
                                       fae_processor_t);
fae_processor_t fae_processor_compose(fae_processor_t,
                                      fae_processor_t);
fae_processor_t fae_processor_loop(fae_processor_t);
fae_processor_t fae_processor_add(fae_type_t);
fae_processor_t fae_processor_subtract(fae_type_t);
fae_processor_t fae_processor_multiply(fae_type_t);
fae_processor_t fae_processor_power(fae_type_t);
fae_processor_t fae_processor_divide(fae_type_t);
fae_processor_t fae_processor_modulo(fae_type_t);
fae_processor_t fae_processor_absolute(fae_type_t);
fae_processor_t fae_processor_not(fae_type_t);
fae_processor_t fae_processor_and(fae_type_t);
fae_processor_t fae_processor_or(fae_type_t);
fae_processor_t fae_processor_xor(fae_type_t);
fae_processor_t fae_processor_bit_not(fae_type_t);
fae_processor_t fae_processor_bit_and(fae_type_t);
fae_processor_t fae_processor_bit_or(fae_type_t);
fae_processor_t fae_processor_bit_xor(fae_type_t);
fae_processor_t fae_processor_shift_left(fae_type_t);
fae_processor_t fae_processor_shift_right(fae_type_t);
fae_processor_t fae_processor_equal(fae_type_t);
fae_processor_t fae_processor_less_than(fae_type_t);
fae_processor_t fae_processor_greater_than(fae_type_t);
fae_processor_t fae_processor_less_than_equal(fae_type_t);
fae_processor_t fae_processor_greater_than_equal(fae_type_t);
fae_processor_t fae_processor_acos(fae_type_t);
fae_processor_t fae_processor_asin(fae_type_t);
fae_processor_t fae_processor_atan(fae_type_t);
fae_processor_t fae_processor_cos(fae_type_t);
fae_processor_t fae_processor_sin(fae_type_t);
fae_processor_t fae_processor_tan(fae_type_t);
fae_processor_t fae_processor_exp(fae_type_t);
fae_processor_t fae_processor_log(fae_type_t);
fae_processor_t fae_processor_log10(fae_type_t);
fae_processor_t fae_processor_pow(fae_type_t);
fae_processor_t fae_processor_sqrt(fae_type_t);
fae_processor_t fae_processor_abs(fae_type_t);
fae_processor_t fae_processor_min(fae_type_t);
fae_processor_t fae_processor_max(fae_type_t);
fae_processor_t fae_processor_fmod(fae_type_t);
fae_processor_t fae_processor_floor(fae_type_t);
fae_processor_t fae_processor_ceil(fae_type_t);
fae_processor_t fae_processor_rint(fae_type_t);

/** @}
    @}
    */

#endif // _FAE_PROCESSOR

