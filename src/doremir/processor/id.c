
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/processor/id.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_id_proc_t
{
    impl_t          impl;                       // Dispatcher
    type_t          type;                       // Type
};

typedef doremir_processor_id_proc_t         this_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

ptr_t id_impl(doremir_id_t interface);

this_t doremir_processor_id_create(type_t type)
{
    this_t proc  = doremir_new(processor_id_proc);
    proc->impl   = &id_impl;
    proc->type   = type;
    return proc;
}

void doremir_processor_id_destroy(this_t proc)
{
    doremir_destroy(proc->type);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

type_t id_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return proc->type;
}

type_t id_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return proc->type;
}

size_t id_buffer_size(frames_t frameSize, ptr_t a)
{
    return doremir_type_size_of(frameSize, id_input_type(a));
}

void id_before(ptr_t a, info_t *info)
{
    // nothing
}

void id_after(ptr_t a, info_t *info)
{
    // nothing
}

void id_process(ptr_t a, info_t *info, samples_t samples)
{
    // nothing
}

// --------------------------------------------------------------------------------

string_t id_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, doremir_string_show(proc->type));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(proc->type));

    return s;
}

void id_destroy(ptr_t a)
{
    doremir_processor_id_destroy(a);
}

ptr_t id_impl(doremir_id_t interface)
{
    static doremir_string_show_t id_show_impl = { id_show };
    static doremir_destroy_t id_destroy_impl = { id_destroy };
    static doremir_processor_interface_t id_processor_interface_impl =
    {
        id_before, id_process, id_after,
        id_input_type, id_output_type, id_buffer_size
    };

    switch (interface)
        {
        case doremir_string_show_i:
            return &id_show_impl;

        case doremir_destroy_i:
            return &id_destroy_impl;

        case doremir_processor_interface_i:
            return &id_processor_interface_impl;

        default:
            return NULL;
        }
}