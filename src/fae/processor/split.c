
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <fae/processor/split.h>
#import <fae/string.h>
#import <fae/util.h>

struct _fae_processor_split_proc_t {
    impl_t              impl;               // Dispatcher
    type_t              input_type;

    size_t              size;               // Number of bytes to copy
};

typedef fae_processor_split_proc_t      this_t;
typedef fae_processor_samples_t         samples_t;
typedef fae_processor_info_t            info_t;

ptr_t split_impl(fae_id_t interface);

inline static bool type_check(string_t *msg, this_t proc)
{
    // Nothing to check
    return true;
}

this_t fae_processor_split_create(type_t type)
{
    this_t proc  = fae_new(processor_split_proc);
    proc->impl = &split_impl;

    proc->input_type = type;

    if (type_check(NULL, proc)) {
        return proc;
    } else {
        assert(false && "Type error");
        // TODO
    }
}

void fae_processor_split_destroy(this_t proc)
{
    fae_delete(proc);
}

// --------------------------------------------------------------------------------

type_t split_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return fae_type_copy(proc->input_type);
}

type_t split_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return fae_type_pair(proc->input_type, proc->input_type);
}

size_t split_buffer_size(frames_t frameSize, ptr_t a)
{
    return fae_type_size_of(frameSize, split_output_type(a));
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

graph_t split_graph(ptr_t a, info_t *info, graph_t graph)
{
    this_t proc = (this_t) a;
    int *offset = &info->buf_offset;
    int *step   = &info->buf_step;
    int *seq    = &info->buf_seq;

    pair_t self  = node_name(*offset,         *step,     *seq);
    pair_t left  = node_name(*offset, (*step) * 2, (*seq) + 1);
    pair_t right = node_name(*offset + *step, (*step) * 2, (*seq) + 1);

    graph = fae_graph_insert(self, graph);
    graph = fae_graph_insert(left, graph);  // insert prematurely
    graph = fae_graph_insert(right, graph);

    graph = fae_graph_connect(self, left, edge_name(*offset), graph);
    graph = fae_graph_connect(self, right, edge_name(*offset + *step), graph);

    return graph;
}


void split_before(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;
    proc->size = fae_type_size_of(info->frame_size, proc->input_type);
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

    s = string_dappend(s, fae_string_show(split_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, fae_string_show(split_output_type(proc)));

    return s;
}

void split_destroy(ptr_t a)
{
    fae_processor_split_destroy(a);
}

ptr_t split_impl(fae_id_t interface)
{
    static fae_string_show_t split_show_impl = { split_show };
    static fae_destroy_t split_destroy_impl = { split_destroy };
    static fae_processor_interface_t split_processor_interface_impl = {
        split_before, split_process, split_after,
        split_input_type, split_output_type, split_buffer_size, split_graph
    };

    switch (interface) {
    case fae_string_show_i:
        return &split_show_impl;

    case fae_destroy_i:
        return &split_destroy_impl;

    case fae_processor_interface_i:
        return &split_processor_interface_impl;

    default:
        return NULL;
    }
}