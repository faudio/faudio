
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <fae/processor/loop.h>
#import <fae/string.h>
#import <fae/util.h>

struct _fae_processor_loop_proc_t {
    impl_t              impl;               // Dispatcher

    proc_t              elem;               // Elements
    proc_interface_t   *elemImpl;           // Fast pointer to the elements' processor implementation

    type_t              bufType;            // Type of loopback buffer
};

typedef fae_processor_loop_proc_t       this_t;
typedef fae_processor_samples_t         samples_t;
typedef fae_processor_info_t            info_t;

ptr_t loop_impl(fae_id_t interface);

inline static bool type_check(string_t *msg, this_t proc)
{
    if (msg) {
        *msg = string("Both input and output must be pair types, and the first component"
                      "of input and output must be the same.");
    }

    return fae_type_is_pair(fae_processor_input_type(proc->elem))
           && fae_type_is_pair(fae_processor_output_type(proc->elem))
           && fae_equal(
               fae_type_get_pair_fst(fae_processor_input_type(proc->elem)),
               fae_type_get_pair_fst(fae_processor_output_type(proc->elem))
           );
}

this_t fae_processor_loop_create(processor_t proc1)
{
    this_t proc     = fae_new(processor_loop_proc);
    proc->impl      = &loop_impl;

    proc->elem      = proc1;
    proc->elemImpl  = fae_interface(fae_processor_interface_i, proc->elem);
    assert(proc->elemImpl && "Must implement Processor");

    proc->bufType   = fae_type_get_pair_fst(fae_processor_input_type(proc->elem)); // TODO get fst

    if (type_check(NULL, proc)) {
        return proc;
    } else {
        assert(false && "Type error");
        // TODO
    }
}

void fae_processor_loop_destroy(this_t proc)
{
    // fae_destroy(proc->elem);
    fae_delete(proc);
}

// --------------------------------------------------------------------------------

type_t loop_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return fae_type_get_pair_fst(fae_processor_input_type(proc->elem));
}

type_t loop_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return fae_type_get_pair_fst(fae_processor_output_type(proc->elem));
}

size_t loop_buffer_size(frames_t frameSize, ptr_t a)
{
    this_t proc = (this_t) a;

    size_t inSize  = fae_type_size_of(frameSize, loop_input_type(a));
    size_t outSize = fae_type_size_of(frameSize, loop_output_type(a));
    size_t loopSize  = fae_type_size_of(frameSize, proc->bufType);

    // FIXME should use buffer size of elements, not type size
    return size_max(inSize + loopSize, outSize + loopSize);
}

graph_t loop_graph(ptr_t a, info_t *info, graph_t graph)
{
    this_t proc = (this_t) a;
    int offset = info->buf_offset;
    int step   = info->buf_step;
    int seq    = info->buf_seq;


    info->buf_loop++;

    // copy %loop (%off+%step)
    // graph = proc->elemImpl->graph(proc->elem, info, graph);
    // copy (%off+%step) %loop

    info->buf_loop--;

    // TODO connections
    // TODO elements

    return graph;
}


void loop_before(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;
    proc->elemImpl->before(proc->elem, info);
}

void loop_after(ptr_t a, info_t *info)
{
    this_t proc = (this_t) a;
    proc->elemImpl->after(proc->elem, info);
}

void loop_process(ptr_t a, info_t *info, samples_t samples)
{
    this_t proc = (this_t) a;
    proc->elemImpl->process(proc->elem, info, samples);
    // TODO flip back to let fb be second (that is offset from second value)
    // We will need to shift memory anyway (?!)
    // Will the memory here be auto-persistent? NO!
}


// --------------------------------------------------------------------------------

string_t loop_show(ptr_t a)
{
    this_t proc = (this_t) a;
    string_t s = string("");

    s = string_dappend(s, fae_string_show(loop_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, fae_string_show(loop_output_type(proc)));

    return s;
}

void loop_destroy(ptr_t a)
{
    fae_processor_loop_destroy(a);
}

ptr_t loop_impl(fae_id_t interface)
{
    static fae_string_show_t loop_show_impl = { loop_show };
    static fae_destroy_t loop_destroy_impl = { loop_destroy };
    static fae_processor_interface_t loop_processor_interface_impl = {
        loop_before, loop_process, loop_after,
        loop_input_type, loop_output_type, loop_buffer_size, loop_graph
    };

    switch (interface) {
    case fae_string_show_i:
        return &loop_show_impl;

    case fae_destroy_i:
        return &loop_destroy_impl;

    case fae_processor_interface_i:
        return &loop_processor_interface_impl;

    default:
        return NULL;
    }
}