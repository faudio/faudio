
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/processor.h>
#include <doremir/processor/id.h>
#include <doremir/processor/const.h>
#include <doremir/processor/unary.h>
#include <doremir/processor/binary.h>
#include <doremir/processor/split.h>
#include <doremir/processor/seq.h>
#include <doremir/processor/par.h>
#include <doremir/processor/loop.h>
#include <doremir/processor/delay.h>
#include <doremir/util.h>

/** Return the input type of the given processor.
 */
doremir_type_t doremir_processor_input_type(doremir_processor_t a)
{
    return ((proc_interface_t *)
            doremir_interface(doremir_processor_interface_i, a))->input_type(a);
}

/** Return the output type of the given processor.
 */
doremir_type_t doremir_processor_output_type(doremir_processor_t a)
{
    return ((proc_interface_t *)
            doremir_interface(doremir_processor_interface_i, a))->output_type(a);
}


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
    return (processor_t)
           doremir_processor_unary_create(input_type, output_type, function, data);
}

/** Lift a binary function to a processor.

    @param input_type1      Type of first input.
    @param input_type2      Type of second input.
    @param output_type      Type of output.
    @param function         Function to be lifted.
    @param data             Value to be passed to function.
 */
doremir_processor_t doremir_processor_binary(doremir_type_t   input_type1,
        doremir_type_t   input_type2,
        doremir_type_t   output_type,
        doremir_binary_t function,
        doremir_ptr_t    data)
{
    return (processor_t)
           doremir_processor_binary_create(input_type1, input_type2, output_type, function, data);
}


/** Create an identity processor.

    This processor returns input of the given type unmodified.

    @param type             Type of input.
 */
doremir_processor_t doremir_processor_identity(doremir_type_t type)
{
    return (processor_t) doremir_processor_id_create(type);
}

/** Create a constant processor.

    This processor consumes input of the given type, and returns a constant value of
    the given type.

    @param input_type       Type of input.
    @param output_type      Type of output.
    @param value
 */
doremir_processor_t doremir_processor_constant(doremir_type_t   input_type,
        doremir_type_t   output_type,
        doremir_ptr_t    value)
{
    return (processor_t) doremir_processor_const_create(input_type, output_type, value);
}

/** Create a delay processor.

    This processor delays its input by the given value.

    @param input_type       Type of input.
    @param samples          Number of samples.
 */
doremir_processor_t doremir_processor_delay(doremir_type_t  type,
        size_t          samples)
{
    assert(false && "Not implemented");
    // return (processor_t) doremir_processor_delay_create(type, samples);
}

/** Create a split processor.
    @param input_type       Type of input.
 */
doremir_processor_t doremir_processor_split(doremir_type_t type)
{
    return (processor_t) doremir_processor_split_create(type);
}

/** Create a processor by combining the given processors in sequence.

    The output type of the first processor must match the input type of the
    second processor, or an error is returned.

        p : `a ~> b`        q : `b ~> c`
        --------------------------------
               p >> q : `a ~> c`.

    @param proc
        First processor in chain.
    @param proc
        Second processor in chain.
    @return
        A new processor, or an error.
 */
doremir_processor_t doremir_processor_seq(doremir_processor_t proc1,
        doremir_processor_t proc2)
{
    return (processor_t)
           doremir_processor_seq_create(proc1, proc2);
}

/** Create a processor by combining the given processors in parallel.

    The input and output types are pairs of the input and output types of the given processors.

        p : `a ~> b`        q : `c ~> d`
        --------------------------------
            p || q : `(a,c) ~> (b,d)`.

    @param proc
        Left processor in hub.
    @param proc
        Right processor in hub.
    @return
        A new processor, or an error.
 */
doremir_processor_t doremir_processor_par(doremir_processor_t proc1,
        doremir_processor_t proc2)
{
    return (processor_t)
           doremir_processor_par_create(proc1, proc2);
}

/** Create a processor by feeding the given processor back into itself.

    The given processor must have inputs and outputs on the form `(a,b)`.

        p : `(a,c) ~> (b,c)`
        ---------------------
         loop p : `a ~> b`.

    @param proc
        Processor to close feedback loop over.
    @return
        A new processor, or an error.
 */
doremir_processor_t doremir_processor_loop(doremir_processor_t proc)
{
    return (processor_t) doremir_processor_loop_create(proc);
}






uint8_t prim_add_i8_i8(ptr_t c, uint8_t a, uint8_t b)
{
    return a + b;
};
float   prim_add_f32_f32(ptr_t c, float a, float b)
{
    return a + b;
};

doremir_processor_t doremir_processor_add(doremir_type_t type)
{
    return doremir_processor_unary(type(uint8), type(uint8), prim_add_i8_i8, NULL);
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




