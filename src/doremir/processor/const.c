
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/processor/const.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_const_proc_t
{
    impl_t          impl;                       // Dispatcher
    type_t          input_type, output_type;    // Type

    ptr_t           value;                      // Constant value
};

typedef doremir_processor_const_proc_t      this_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

ptr_t const_impl(doremir_id_t interface);

this_t doremir_processor_const_create(type_t type1, type_t type2, ptr_t value)
{
    this_t proc       = doremir_new(processor_const_proc);
    proc->impl        = &const_impl;

    proc->input_type  = type1;
    proc->output_type = type2;
    proc->value       = value;

    return proc;
}

void doremir_processor_const_destroy(this_t proc)
{
    doremir_destroy(proc->input_type);
    doremir_destroy(proc->output_type);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

type_t const_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return proc->input_type;
}

type_t const_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return proc->output_type;
}

size_t const_buffer_size(frames_t frameSize, ptr_t a)
{
    size_t inSize  = doremir_type_size_of(frameSize, const_input_type(a));
    size_t outSize = doremir_type_size_of(frameSize, const_output_type(a));
    return size_max(inSize, outSize);
}

void const_before(ptr_t a, info_t *info)
{
    // nothing
}

void const_after(ptr_t a, info_t *info)
{
    // nothing
}

void const_process(ptr_t a, info_t *info, samples_t samples)
{
    // TODO copy value
}

// --------------------------------------------------------------------------------

string_t const_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, doremir_string_show(proc->input_type));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(proc->output_type));

    return s;
}

void const_destroy(ptr_t a)
{
    doremir_processor_const_destroy(a);
}

ptr_t const_impl(doremir_id_t interface)
{
    static doremir_string_show_t const_show_impl = { const_show };
    static doremir_destroy_t const_destroy_impl = { const_destroy };
    static doremir_processor_interface_t const_processor_interface_impl =
    {
        const_before, const_process, const_after,
        const_input_type, const_output_type, const_buffer_size
    };

    switch (interface)
        {
        case doremir_string_show_i:
            return &const_show_impl;

        case doremir_destroy_i:
            return &const_destroy_impl;

        case doremir_processor_interface_i:
            return &const_processor_interface_impl;

        default:
            return NULL;
        }
}