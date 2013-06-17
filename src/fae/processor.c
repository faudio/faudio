
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/processor.h>
#include <fae/processor/unary.h>
#include <fae/processor/binary.h>
#include <fae/processor/split.h>
#include <fae/processor/seq.h>
#include <fae/processor/par.h>
#include <fae/processor/loop.h>
#include <fae/processor/delay.h>
#include <fae/system/directory.h>
#include <fae/util.h>


/** Return the input type of the given processor.

    @param proc             A processor.
 */
fae_type_t fae_processor_input_type(fae_processor_t proc)
{
    assert(fae_interface(fae_processor_interface_i, proc) && "Must implement Processor");
    return ((proc_interface_t *) fae_interface(fae_processor_interface_i, proc))->input_type(proc);
}

/** Return the output type of the given processor.

    @param proc             A processor.
 */
fae_type_t fae_processor_output_type(fae_processor_t proc)
{
    assert(fae_interface(fae_processor_interface_i, proc) && "Must implement Processor");
    return ((proc_interface_t *) fae_interface(fae_processor_interface_i, proc))->output_type(proc);
}

/** Return the output type of the given processor.
    @param proc             A processor.
 */
size_t fae_processor_buffer_size(fae_type_frames_t frames, fae_processor_t proc)
{
    assert(fae_interface(fae_processor_interface_i, proc) && "Must implement Processor");
    return ((proc_interface_t *) fae_interface(fae_processor_interface_i, proc))->buffer_size(frames, proc);
}

fae_graph_t fae_processor_graph(fae_processor_t proc,
                                        fae_processor_info_t *info,
                                        fae_graph_t graph)
{
    assert(fae_interface(fae_processor_interface_i, proc) && "Must implement Processor");
    return ((proc_interface_t *) fae_interface(fae_processor_interface_i, proc))->graph(proc, info, graph);
}

/** Returns the address of the given processor.
    @param proc             A processor.
 */
fae_ptr_t fae_processor_address(fae_processor_t proc)
{
    return proc;
}

/** Write a graph representation of the given processor
    (in the dot language) to the given file.
 */
void fae_processor_write_graph(fae_processor_t proc,
                                   fae_string_file_path_t path)
{
    graph_t graph = fae_graph_empty();
    fae_processor_info_t info = {
        .buf_offset = 0,
        .buf_step   = 1,
        .buf_loop   = 1,
        .buf_seq    = 1
    };
    graph = fae_processor_graph(proc, &info, graph);
    fae_system_directory_write_file(path, fae_graph_to_dot(string(""), string(""), graph));
}


inline static void *identity(void *x, void *a)
{
    return a;
}
inline static void *constant(void *x, void *a)
{
    return x;
}

/** Create an identity processor.

    This processor returns input of the given type unmodified.

    @param type             Type of input.
    @return                 A processor.
 */
fae_processor_t fae_processor_identity(fae_type_t type)
{
    return (processor_t) fae_processor_unary_create(type, type, identity, NULL);
}

/** Create a constant processor.

    This processor consumes input of the given type, and returns a constant value of
    the given type.

    @param input_type       Type of input.
    @param output_type      Type of output.
    @param value            Pointer to a buffer containing the value.
 */
fae_processor_t fae_processor_constant(fae_type_t   input_type,
                                               fae_type_t   output_type,
                                               fae_ptr_t    value)
{
    // TODO defensively copy, or trust the user?
    // TODO if we are closing over a frame, should we multiply here instead of trusting the user to?
    return (processor_t) fae_processor_unary_create(input_type, output_type, constant, NULL);
}

/** Create a delay processor.

    This processor delays its input by the given value.

    @param input_type       Type of input.
    @param samples          Number of samples.
 */
fae_processor_t fae_processor_delay(fae_type_t  type,
                                            size_t          samples)
{
    assert(false && "Not implemented");
}

/** Create a split processor.
    @param input_type       Type of input.
 */
fae_processor_t fae_processor_split(fae_type_t type)
{
    return (processor_t) fae_processor_split_create(type);
}

/** Lift a unary function to a processor.

    The given types must be simple types or frames types.

    The given function will receive a pointer to a single value of the given input
    types and is expected to return a pointer to a value of the proper output type.
    The received pointer may be used as return value.

    @param input_type       Input type. Must be unit, a simple type or a frame type containing a simple type.
    @param output_type      Type of output. Must be unit, a simple type or a frame type containing a simple type.
    @param function         Function to be lifted.
    @param data             Value to be passed to function.
    @return                 A processor.
 */
fae_processor_t fae_processor_unary
(
    fae_type_t  input_type,
    fae_type_t  output_type,
    fae_unary_t function,
    fae_ptr_t   data
)
{
    return (processor_t)
           fae_processor_unary_create(
               input_type, output_type,
               function, data
           );
}

/** Lift a binary function to a processor.

    The given types must be simple types or frames types.

    The given function will receive pointers to single values of the given input
    types and is expected to return a pointer to a value of the proper output type.
    The first received pointer may be used as return value.

    @param input_type1      Type of first input.
    @param input_type2      Type of second input.
    @param output_type      Type of output.
    @param function         Function to be lifted.
    @param data             Value to be passed to function.
    @return                 A processor.
 */
fae_processor_t fae_processor_binary
(
    fae_type_t   input_type1,
    fae_type_t   input_type2,
    fae_type_t   output_type,
    fae_binary_t function,
    fae_ptr_t    data
)
{
    return (processor_t)
           fae_processor_binary_create(
               input_type1, input_type2, output_type,
               function, data
           );
}


/** Create a processor by composing the given processors in parallel.

    The input and output types are pairs of the input and output types of the given processors.

        \f[
            \frac
                {p \in A \rightarrow B \qquad q \in C \rightarrow D}
                {par(p,q) \in (A,C) \rightarrow (B,D)}
        \f]

    @param proc             Left processor.
    @param proc             Right processor.
    @return                 A new processor, or an error.
 */
fae_processor_t fae_processor_parallel(fae_processor_t proc1,
                                               fae_processor_t proc2)
{
    return (processor_t)
           fae_processor_par_create(proc1, proc2);
}

/** Create a processor by composing the given processors in sequence.

    The output type of the first processor must match the input type of the
    second processor, or an error is returned.

        \f[
            \frac
                {p \in A \rightarrow B \qquad q \in B \rightarrow C}
                {seq(p,q) \in A \rightarrow C}
        \f]

    @param proc             First processor.
    @param proc             Second processor.
    @return                 A new processor, or an error.
 */
fae_processor_t fae_processor_sequence(fae_processor_t proc1,
                                               fae_processor_t proc2)
{
    return (processor_t) fae_processor_seq_create(proc1, proc2);
}

/** Create a processor by composing the given processors in reverse order.
    This function is analogous to function composition.

    The output type of the second processor must match the input type of the
    first processor, or an error is returned.

        \f[
            \frac
                {p \in B \rightarrow C \qquad q \in A \rightarrow B}
                {compose(p,q) \in A \rightarrow C}
        \f]

    @param proc             First processor.
    @param proc             Second processor.
    @return                 A new processor, or an error.
 */
fae_processor_t fae_processor_compose(fae_processor_t proc1, fae_processor_t proc2)
{
    return (processor_t) fae_processor_seq_create(proc2, proc1);
}

/** Create a processor by feeding the given processor back into itself.

        \f[
            \frac
                {p \in (C,A) \rightarrow (C,B) }
                { loop(p) \in A \rightarrow B }
        \f]

    @param proc             Processor to close feedback loop over.
    @return                 A new processor, or an error.
 */
fae_processor_t fae_processor_loop(fae_processor_t proc)
{
    return (processor_t) fae_processor_loop_create(proc);
}







uint8_t prim_add_i8_i8(ptr_t c, uint8_t a, uint8_t b)
{
    return a + b;
};
float   prim_add_f32_f32(ptr_t c, float a, float b)
{
    return a + b;
};

fae_processor_t fae_processor_add(fae_type_t type)
{
    return (processor_t) fae_processor_unary(type(i8), type(i8), (unary_t) prim_add_i8_i8, NULL);
}

fae_processor_t fae_processor_subtract(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_multiply(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_divide(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_modulo(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_absolute(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_not(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_and(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_or(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_xor(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_bit_not(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_bit_and(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_bit_or(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_bit_xor(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_shift_left(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_shift_right(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_equal(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_less_than(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_greater_than(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_less_than_equal(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_greater_than_equal(fae_type_t type)
{
    assert(false && "Not implemented");
}


inline static void *apply_1f21f(void *f, void *a)
{
    typedef float(*f2f)(float);
    f2f     g = (f2f) f;
    float  *b = a;
    b[0] = g(b[0]);
    return a;
}

fae_processor_t fae_processor_acos(fae_type_t type)
{
    // assert float, double or long double
    return fae_processor_unary(type(f32), type(f32), apply_1f21f, acos);
}

fae_processor_t fae_processor_asin(fae_type_t type)
{
    return fae_processor_unary(type(f32), type(f32), apply1, asin);
}

fae_processor_t fae_processor_atan(fae_type_t type)
{
    return fae_processor_unary(type(f32), type(f32), apply1, atan);
}

fae_processor_t fae_processor_cos(fae_type_t type)
{
    return fae_processor_unary(type(f32), type(f32), apply1, cos);
}

fae_processor_t fae_processor_sin(fae_type_t type)
{
    return fae_processor_unary(type(f32), type(f32), apply1, sin);
}

fae_processor_t fae_processor_tan(fae_type_t type)
{
    return fae_processor_unary(type(f32), type(f32), apply1, tan);
}

fae_processor_t fae_processor_exp(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_log(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_log10(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_pow(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_sqrt(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_abs(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_min(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_max(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_fmod(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_remainder(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_floor(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_ceil(fae_type_t type)
{
    assert(false && "Not implemented");
}

fae_processor_t fae_processor_rint(fae_type_t type)
{
    assert(false && "Not implemented");
}





