
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/processor/split.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_split_proc_t {
    impl_t              impl;               // Dispatcher
    type_t              input_type;

    size_t              size;               // Number of bytes to copy
};

typedef doremir_processor_split_proc_t      this_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

ptr_t split_impl(doremir_id_t interface);

inline static bool type_check(string_t *msg, this_t proc)
{
    // Nothing to check
    return true;
}

this_t doremir_processor_split_create(type_t type)
{
    this_t proc  = doremir_new(processor_split_proc);
    proc->impl = &split_impl;

    proc->input_type = type;

    if (type_check(NULL, proc)) {
        return proc;
    } else {
        assert(false && "Type error");
        // TODO
    }
}

void doremir_processor_split_destroy(this_t proc)
{
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

type_t split_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return doremir_type_copy(proc->input_type);
}

type_t split_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return doremir_type_pair(proc->input_type, proc->input_type);
}

size_t split_buffer_size(frames_t frameSize, ptr_t a)
{
    return doremir_type_size_of(frameSize, split_output_type(a));
}

graph_t split_graph(ptr_t a, info_t *info, graph_t graph)
{
    this_t proc = (this_t) a;
    // TODO
}


void split_before(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;
    proc->size = doremir_type_size_of(info->frame_size, proc->input_type);
}

void split_after(ptr_t a, info_t *info)
{
    // nothing
}

void split_process(ptr_t a, info_t *info, samples_t samples)
{
    this_t proc = (this_t) a;

    size_t sz = proc->size;
    // memcpy(output, input, sz);
    // memcpy(output + sz, input, sz);
}

// --------------------------------------------------------------------------------

string_t split_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, doremir_string_show(split_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(split_output_type(proc)));

    return s;
}

void split_destroy(ptr_t a)
{
    doremir_processor_split_destroy(a);
}

ptr_t split_impl(doremir_id_t interface)
{
    static doremir_string_show_t split_show_impl = { split_show };
    static doremir_destroy_t split_destroy_impl = { split_destroy };
    static doremir_processor_interface_t split_processor_interface_impl = {
        split_before, split_process, split_after,
        split_input_type, split_output_type, split_buffer_size, split_graph
    };

    switch (interface) {
    case doremir_string_show_i:
        return &split_show_impl;

    case doremir_destroy_i:
        return &split_destroy_impl;

    case doremir_processor_interface_i:
        return &split_processor_interface_impl;

    default:
        return NULL;
    }
}