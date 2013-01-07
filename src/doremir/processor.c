
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/processor.h>
#include <doremir/util.h>

void doremir_processor_info_default(doremir_processor_info_t *info)
{
    // info->sample_rate  = 44100;
    // info->sample_count = 0;
    // info->vector_size  = 1024;
    // info->real_time    = seconds(0);
}

doremir_processor_any_t doremir_processor_unary(doremir_type_t type,
                                                doremir_unary_t function)
{
    // doremir_processor_unary_create(type, function);
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_binary(doremir_type_t type,
                                                 doremir_binary_t function)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_ternary(doremir_type_t type,
                                                  doremir_ternary_t function)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_identity(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_constant(doremir_type_t type1,
                                                   doremir_type_t type2,
                                                   doremir_ptr_t value)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_delay(doremir_type_t type,
                                                size_t samples)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_split(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_sequence(doremir_processor_any_t proc1,
                                                   doremir_processor_any_t proc2)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_parallel(doremir_processor_any_t proc1,
                                                   doremir_processor_any_t proc2)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_loop(doremir_processor_any_t proc)
{
    assert(false && "Not implemented");
}

#define WRAP_UNARY(F) \
    doremir_processor_any_t doremir_processor_##F(doremir_type_t type) \
    {                                                                  \
        return doremir_processor_unary(type, (doremir_unary_t) F);     \
    }

WRAP_UNARY(cos);
WRAP_UNARY(sin);
WRAP_UNARY(tan);
WRAP_UNARY(acos);
WRAP_UNARY(asin);
WRAP_UNARY(atan);


doremir_processor_any_t doremir_processor_add(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_subtract(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_multiply(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_divide(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_modulo(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_absolute(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_and(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_or(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_not(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_bit_and(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_bit_or(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_bit_not(doremir_type_t type)
{
    assert(false && "Not implemented");
}

doremir_processor_any_t doremir_processor_bit_xor(doremir_type_t type)
{
    assert(false && "Not implemented");
}




