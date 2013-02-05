
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/processor/unary.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_unary_proc_t {
    impl_t          impl;                       // Dispatcher
    type_t          input_type, output_type;    // Types

    unary_t         function;                   // Lifted function and closure
    ptr_t           data;
};

typedef doremir_processor_unary_proc_t      this_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

ptr_t unary_impl(doremir_id_t interface);

this_t doremir_processor_unary_create
(
    type_t  type1,
    type_t  type2,
    unary_t function,
    ptr_t   data
)
{
    this_t proc       = doremir_new(processor_unary_proc);
    proc->impl        = &unary_impl;

    proc->input_type  = type1;
    proc->output_type = type2;

    proc->function    = function;
    proc->data        = data;

    return proc;
}

void doremir_processor_unary_destroy(this_t proc)
{
    doremir_destroy(proc->input_type);
    doremir_destroy(proc->output_type);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

type_t unary_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return proc->input_type;
}

type_t unary_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return proc->output_type;
}

size_t unary_buffer_size(frames_t frameSize, ptr_t a)
{
    size_t inSize  = doremir_type_size_of(frameSize, unary_input_type(a));
    size_t outSize = doremir_type_size_of(frameSize, unary_output_type(a));
    return size_max(inSize, outSize);
}

void unary_before(ptr_t a, info_t *info)
{
    // nothing
}

void unary_after(ptr_t a, info_t *info)
{
    // nothing
}

void unary_process(ptr_t a, info_t *info, samples_t samples)
{
    // unary_proc_uint8_t_uint8_t(1, proc2, samples, samples);
    // TODO call
}

// --------------------------------------------------------------------------------

string_t unary_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, doremir_string_show(proc->input_type));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(proc->output_type));

    return s;
}

void unary_destroy(ptr_t a)
{
    doremir_processor_unary_destroy(a);
}

ptr_t unary_impl(doremir_id_t interface)
{
    static doremir_string_show_t unary_show_impl = { unary_show };
    static doremir_destroy_t unary_destroy_impl = { unary_destroy };
    static doremir_processor_interface_t unary_processor_interface_impl = {
        unary_before, unary_process, unary_after,
        unary_input_type, unary_output_type, unary_buffer_size
    };

    switch (interface) {
    case doremir_string_show_i:
        return &unary_show_impl;

    case doremir_destroy_i:
        return &unary_destroy_impl;

    case doremir_processor_interface_i:
        return &unary_processor_interface_impl;

    default:
        return NULL;
    }
}