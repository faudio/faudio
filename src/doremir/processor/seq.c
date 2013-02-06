
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/processor/seq.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_seq_proc_t {
    impl_t              impl;               // Dispatcher

    proc_t              elem[2];            // Elements
    proc_interface_t   *elemImpl[2];        // Fast impl pointer
};

typedef doremir_processor_seq_proc_t    this_t;
typedef doremir_processor_samples_t     samples_t;
typedef doremir_processor_info_t        info_t;

ptr_t seq_impl(doremir_id_t interface);

inline static bool type_check(string_t *msg, this_t proc)
{
    if (msg) {
        *msg = string("Input type must equal output type");
    }

    return doremir_equal(
               doremir_processor_output_type(proc->elem[0]),
               doremir_processor_input_type(proc->elem[1])
           );
}

this_t doremir_processor_seq_create(processor_t proc1, processor_t proc2)
{
    this_t proc         = doremir_new(processor_seq_proc);
    proc->impl          = &seq_impl;

    proc->elem[0]       = proc1;
    proc->elem[1]       = proc2;

    proc->elemImpl[0]   = doremir_interface(doremir_processor_interface_i, proc->elem[0]);
    proc->elemImpl[1]   = doremir_interface(doremir_processor_interface_i, proc->elem[1]);
    assert(proc->elemImpl[0] && "Must implement Processor");
    assert(proc->elemImpl[1] && "Must implement Processor");

    if (type_check(NULL, proc)) {
        return proc;
    } else {
        assert(false && "Type error");
        // TODO
    }
}

void doremir_processor_seq_destroy(this_t proc)
{
    doremir_destroy(proc->elem[0]);
    doremir_destroy(proc->elem[1]);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

type_t seq_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return doremir_processor_input_type(proc->elem[0]);
}

type_t seq_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return doremir_processor_output_type(proc->elem[1]);
}

size_t seq_buffer_size(frames_t frameSize, ptr_t a)
{
    size_t inSize  = doremir_type_size_of(frameSize, seq_input_type(a));
    size_t outSize = doremir_type_size_of(frameSize, seq_output_type(a));
    return size_max(inSize, outSize);
    // FIXME should use buffer size of elements, not type size
}

graph_t seq_graph(ptr_t a, info_t *info, graph_t graph)
{
    this_t proc = (this_t) a;

    // TODO connections

    graph = proc->elemImpl[0]->graph(proc->elem[0], info, graph);
    info->buf_seq++;
    graph = proc->elemImpl[1]->graph(proc->elem[1], info, graph);
    info->buf_seq--;

    return graph;
}


void seq_before(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;

    proc->elemImpl[0]->before(proc->elem[0], info);
    proc->elemImpl[1]->before(proc->elem[1], info);
}

void seq_after(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;

    proc->elemImpl[0]->after(proc->elem[0], info);
    proc->elemImpl[1]->after(proc->elem[1], info);
}

void seq_process(ptr_t a, info_t *info, samples_t samples)
{
    this_t proc = (this_t) a;

    proc->elemImpl[0]->process(proc->elem[0], info, samples);
    proc->elemImpl[1]->process(proc->elem[1], info, samples);
}

// --------------------------------------------------------------------------------

string_t seq_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, doremir_string_show(seq_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(seq_output_type(proc)));

    return s;
}

void seq_destroy(ptr_t a)
{
    doremir_processor_seq_destroy(a);
}

ptr_t seq_impl(doremir_id_t interface)
{
    static doremir_string_show_t seq_show_impl = { seq_show };
    static doremir_destroy_t seq_destroy_impl = { seq_destroy };
    static doremir_processor_interface_t seq_processor_interface_impl = {
        seq_before, seq_process, seq_after,
        seq_input_type, seq_output_type, seq_buffer_size, seq_graph
    };

    switch (interface) {
    case doremir_string_show_i:
        return &seq_show_impl;

    case doremir_destroy_i:
        return &seq_destroy_impl;

    case doremir_processor_interface_i:
        return &seq_processor_interface_impl;

    default:
        return NULL;
    }
}