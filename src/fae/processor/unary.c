
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <fae/processor/unary.h>
#import <fae/string.h>
#import <fae/util.h>

struct _fae_processor_unary_proc_t {
    impl_t          impl;                       // Dispatcher
    type_t          input_type, output_type;    // Types

    unary_t         function;                   // Lifted function and closure
    ptr_t           data;
};

typedef fae_processor_unary_proc_t      this_t;
typedef fae_processor_samples_t         samples_t;
typedef fae_processor_info_t            info_t;

ptr_t unary_impl(fae_id_t interface);

this_t fae_processor_unary_create
(
    type_t  type1,
    type_t  type2,
    unary_t function,
    ptr_t   data
)
{
    this_t proc       = fae_new(processor_unary_proc);
    proc->impl        = &unary_impl;

    proc->input_type  = type1;
    proc->output_type = type2;

    proc->function    = function;
    proc->data        = data;

    return proc;
}

void fae_processor_unary_destroy(this_t proc)
{
    // fae_destroy(proc->input_type);
    // fae_destroy(proc->output_type);
    fae_delete(proc);
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
    size_t inSize  = fae_type_size_of(frameSize, unary_input_type(a));
    size_t outSize = fae_type_size_of(frameSize, unary_output_type(a));
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

graph_t unary_graph(ptr_t a, info_t *info, graph_t graph)
{
    this_t proc = (this_t) a;
    int *offset = &info->buf_offset;
    int *step   = &info->buf_step;
    int *seq    = &info->buf_seq;

    pair_t self  = node_name(*offset, *step, *seq);
    graph = fae_graph_insert(self, graph);

    return graph;
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
    this_t proc = (this_t) a;

    int offset = 0; // TODO
    void *input  = ((void **) samples)[offset];
    void *output = ((void **) samples)[offset];
    void *result;

    size_t size = info->frame_size; // or 1

    for (int samp = 0; samp < size; ++samp) {
        result = proc->function(proc->data, input);

        if (result != output) {
            // copy from res to output
        }
    }
}

// --------------------------------------------------------------------------------

string_t unary_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, fae_string_show(proc->input_type));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, fae_string_show(proc->output_type));

    return s;
}

void unary_destroy(ptr_t a)
{
    fae_processor_unary_destroy(a);
}

ptr_t unary_impl(fae_id_t interface)
{
    static fae_string_show_t unary_show_impl = { unary_show };
    static fae_destroy_t unary_destroy_impl = { unary_destroy };
    static fae_processor_interface_t unary_processor_interface_impl = {
        unary_before, unary_process, unary_after,
        unary_input_type, unary_output_type, unary_buffer_size, unary_graph
    };

    switch (interface) {
    case fae_string_show_i:
        return &unary_show_impl;

    case fae_destroy_i:
        return &unary_destroy_impl;

    case fae_processor_interface_i:
        return &unary_processor_interface_impl;

    default:
        return NULL;
    }
}