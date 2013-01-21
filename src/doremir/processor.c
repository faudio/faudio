
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/processor.h>
#include <doremir/processor/unary.h>
#include <doremir/processor/binary.h>
#include <doremir/processor/split.h>
#include <doremir/processor/seq.h>
#include <doremir/processor/par.h>
#include <doremir/processor/loop.h>
#include <doremir/processor/delay.h>
#include <doremir/util.h>

// void doremir_processor_info_default(doremir_processor_info_t *info)
// {
    // info->sample_rate  = 44100;
    // info->sample_count = 0;
    // info->vector_size  = 1024;
    // info->real_time    = seconds(0);
// }

/** Lift a unary function to a processor.
    @param input_type       Input type.
    @param output_type      Type of output.
    @param function         Function to be lifted.
    @param data             Value to be passed to function.
 */
doremir_processor_t doremir_processor_unary(doremir_type_t  input_type,
                                            doremir_type_t  output_type,
                                            doremir_unary_t function,
                                            doremir_ptr_t   data)
{
    return (doremir_processor_t) doremir_processor_unary_create(input_type, output_type, function, data);
}

/** Create a binary processor.
    @param input_type1      Type of first input.
    @param input_type2      Type of second input.
    @param output_type      Type of output.
    @param function         Function to be lifted.
    @param data             Value to be passed to function.
 */
doremir_processor_t doremir_processor_binary(doremir_type_t   input_type1,
                                             doremir_type_t   input_type2,
                                             doremir_type_t   output_type2,
                                             doremir_binary_t function,
                                             doremir_ptr_t    data)
{
    assert(false && "Not implemented");
}

// doremir_processor_t doremir_processor_ternary(doremir_type_t type1,
//                                               doremir_type_t type2,
//                                               doremir_type_t type3,
//                                               doremir_type_t type4,
//                                               doremir_ternary_t function)
// {
//     assert(false && "Not implemented");
// }

/** Create an identity processor.
    @param type             Type of input.
 */
doremir_processor_t doremir_processor_identity(doremir_type_t type)
{
    assert(false && "Not implemented");
}

/** Create a constant processor.
    @param input_type       Type of input.
    @param output_type      Type of output.
    @param value
 */
doremir_processor_t doremir_processor_constant(doremir_type_t   input_type,
                                               doremir_type_t   output_type,
                                               doremir_ptr_t    value)
{
    assert(false && "Not implemented");
}

/** Create a delay processor.
    @param input_type       Type of input.
    @param samples          Number of samples.
 */
doremir_processor_t doremir_processor_delay(doremir_type_t  type,
                                            size_t          samples)
{
    assert(false && "Not implemented");
}

/** Create a split processor.
    @param input_type       Type of input.
 */
doremir_processor_t doremir_processor_split(doremir_type_t type)
{
    assert(false && "Not implemented");
}

/** Create a processor by combining the given processors in sequence.
    @param proc1
    @param proc2
 */
doremir_processor_t doremir_processor_seq(doremir_processor_t proc1,
                                          doremir_processor_t proc2)
{
    assert(false && "Not implemented");
}

/** Create a processor by combining the given processors in parallel.
    @param proc1
    @param proc2
 */
doremir_processor_t doremir_processor_par(doremir_processor_t proc1,
                                          doremir_processor_t proc2)
{
    assert(false && "Not implemented");
}

/** Create a processor by feeding the given processor back into itself.
    @param proc
 */
doremir_processor_t doremir_processor_loop(doremir_processor_t proc)
{
    assert(false && "Not implemented");
}

/*
#define WRAP_UNARY(F,P) \
    doremir_processor_t doremir_processor_##F(doremir_type_t input_type, doremir_type_t output_type) \
    {                                                                  \
        return doremir_processor_unary(input_type, output_type, (doremir_unary_t) F);     \
    }

WRAP_UNARY(cos);
WRAP_UNARY(sin);
WRAP_UNARY(tan);
WRAP_UNARY(acos);
WRAP_UNARY(asin);
WRAP_UNARY(atan);*/



doremir_processor_t doremir_processor_add(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_subtract(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_multiply(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_divide(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_modulo(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_absolute(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_and(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_or(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_not(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_bit_and(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_bit_or(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_bit_not(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_t doremir_processor_bit_xor(doremir_type_t type)
{
    assert(false && "Not implemented");
}




