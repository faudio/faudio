
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/processor/par.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_par_proc_t {
    impl_t              impl;                   // Dispatcher

    proc_t              elem[2];                // Elements
    proc_interface_t   *elemImpl[2];            // Fast pointer to the elements' processor implementation

    size_t              inOffset, outOffset;
};

typedef doremir_processor_par_proc_t        this_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

ptr_t par_impl(doremir_id_t interface);

inline static bool type_check(string_t *msg, this_t proc)
{
    // Nothing to check
    return true;
}

this_t doremir_processor_par_create(processor_t proc1, processor_t proc2)
{
    this_t proc         = doremir_new(processor_par_proc);
    proc->impl          = &par_impl;

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

void doremir_processor_par_destroy(this_t proc)
{
    doremir_destroy(proc->elem[0]);
    doremir_destroy(proc->elem[1]);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

type_t par_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    type_t t0 = doremir_processor_input_type(proc->elem[0]);
    type_t t1 = doremir_processor_input_type(proc->elem[1]);
    return doremir_type_pair(t0, t1);
}

type_t par_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    type_t t0 = doremir_processor_output_type(proc->elem[0]);
    type_t t1 = doremir_processor_output_type(proc->elem[1]);
    return doremir_type_pair(t0, t1);
}

size_t par_buffer_size(frames_t frameSize, ptr_t a)
{
    size_t inSize  = doremir_type_size_of(frameSize, par_input_type(a));
    size_t outSize = doremir_type_size_of(frameSize, par_output_type(a));
    return size_max(inSize, outSize);
    // FIXME should use buffer size of elements, not type size
}

graph_t par_graph(ptr_t a, info_t *info, graph_t graph)
{
    this_t proc = (this_t) a;
    int *offset = &info->buf_offset;
    int *step   = &info->buf_step;
    int *seq    = &info->buf_seq;

    // TODO connections

    *step *= 2;

    graph = proc->elemImpl[0]->graph(proc->elem[0], info, graph);

    *offset += (*step/2);
    graph = proc->elemImpl[1]->graph(proc->elem[1], info, graph);
    *offset -= (*step/2);

    *step /= 2;

    return graph;
}


void par_before(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;

    proc->elemImpl[0]->before(proc->elem[0], info);
    proc->elemImpl[1]->before(proc->elem[1], info);

    // Calculate offsets
    proc->inOffset  = doremir_type_offset_of(info->frame_size, par_input_type(proc));
    proc->outOffset = doremir_type_offset_of(info->frame_size, par_output_type(proc));
}

void par_after(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;

    proc->elemImpl[0]->after(proc->elem[0], info);
    proc->elemImpl[1]->after(proc->elem[1], info);
}

void par_process(ptr_t a, info_t *info, samples_t samples)
{
    this_t proc = (this_t) a;

}

// --------------------------------------------------------------------------------

string_t par_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, doremir_string_show(par_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(par_output_type(proc)));

    return s;
}

void par_destroy(ptr_t a)
{
    doremir_processor_par_destroy(a);
}

ptr_t par_impl(doremir_id_t interface)
{
    static doremir_string_show_t par_show_impl = { par_show };
    static doremir_destroy_t par_destroy_impl = { par_destroy };
    static doremir_processor_interface_t par_processor_interface_impl = {
        par_before, par_process, par_after,
        par_input_type, par_output_type, par_buffer_size, par_graph
    };

    switch (interface) {
    case doremir_string_show_i:
        return &par_show_impl;

    case doremir_destroy_i:
        return &par_destroy_impl;

    case doremir_processor_interface_i:
        return &par_processor_interface_impl;

    default:
        return NULL;
    }
}