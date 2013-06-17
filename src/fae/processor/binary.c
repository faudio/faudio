
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <fae/processor/binary.h>
#import <fae/string.h>
#import <fae/util.h>

struct _fae_processor_binary_proc_t {
    impl_t          impl;                           // Dispatcher
    type_t          input_type[2], output_type;     // Types

    binary_t        function;                       // Lifted function and closure
    ptr_t           data;
};

typedef fae_processor_binary_proc_t     this_t;
typedef fae_processor_samples_t         samples_t;
typedef fae_processor_info_t            info_t;

ptr_t binary_impl(fae_id_t interface);

this_t fae_processor_binary_create
(
    type_t   type1,
    type_t   type2,
    type_t   type3,
    binary_t function,
    ptr_t    data
)
{
    this_t proc             = fae_new(processor_binary_proc);
    proc->impl              = &binary_impl;

    proc->input_type[0]     = type1;
    proc->input_type[1]     = type2;
    proc->output_type       = type3;

    proc->function          = function;
    proc->data              = data;

    return proc;
}

void fae_processor_binary_destroy(this_t proc)
{
    // fae_destroy(proc->input_type);
    // fae_destroy(proc->output_type);
    fae_delete(proc);
}

// --------------------------------------------------------------------------------

type_t binary_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return fae_type_pair(proc->input_type[0], proc->input_type[1]);
}

type_t binary_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return proc->output_type;
}

size_t binary_buffer_size(frames_t frameSize, ptr_t a)
{
    size_t inSize  = fae_type_size_of(frameSize, binary_input_type(a));
    size_t outSize = fae_type_size_of(frameSize, binary_output_type(a));
    return size_max(inSize, outSize);
}

static inline string_t node_name(int off, int step, int seq)
{
    char name[50];
    snprintf(name, 50, "node_%d_%d_%d", off, step, seq);
    return string(name);
}
static inline string_t edge_name(int off)
{
    return format_integral("(%d)", off);
}

graph_t binary_graph(ptr_t a, info_t *info, graph_t graph)
{
    this_t proc = (this_t) a;
    int *offset = &info->buf_offset;
    int *step   = &info->buf_step;
    int *seq    = &info->buf_seq;

    pair_t self  = node_name(*offset, *step, *seq);
    graph = fae_graph_insert(self, graph);

    return graph;
}


void binary_before(ptr_t a, info_t *info)
{
    // nothing
}

void binary_after(ptr_t a, info_t *info)
{
    // nothing
}

void binary_process(ptr_t a, info_t *info, samples_t samples)
{
    // TODO call func
}

// --------------------------------------------------------------------------------

string_t binary_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, string("("));
    s = string_dappend(s, fae_string_show(proc->input_type[0]));
    s = string_dappend(s, string(","));
    s = string_dappend(s, fae_string_show(proc->input_type[1]));
    s = string_dappend(s, string(") ~> "));
    s = string_dappend(s, fae_string_show(proc->output_type));

    return s;
}

void binary_destroy(ptr_t a)
{
    fae_processor_binary_destroy(a);
}

ptr_t binary_impl(fae_id_t interface)
{
    static fae_string_show_t binary_show_impl = { binary_show };
    static fae_destroy_t binary_destroy_impl = { binary_destroy };
    static fae_processor_interface_t binary_processor_interface_impl = {
        binary_before, binary_process, binary_after,
        binary_input_type, binary_output_type, binary_buffer_size, binary_graph
    };

    switch (interface) {
    case fae_string_show_i:
        return &binary_show_impl;

    case fae_destroy_i:
        return &binary_destroy_impl;

    case fae_processor_interface_i:
        return &binary_processor_interface_impl;

    default:
        return NULL;
    }
}