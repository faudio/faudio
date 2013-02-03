
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#import <doremir/processor/loop.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_loop_proc_t {
    impl_t              impl;               // Dispatcher

    proc_t              elem;               // Elements
    proc_interface_t  * elemImpl;           // Fast pointer to the elements' processor implementation

    type_t              bufType;            // Type of loopback buffer
    // TODO add buffer
};

typedef doremir_processor_loop_proc_t       this_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

ptr_t loop_impl(doremir_id_t interface);

inline static bool type_check(string_t * msg, this_t proc)
{
    if (msg) {
        *msg = string("Both input and output must be pair types, and the first component"
                      "of input and output must be the same.");
    }

    return doremir_type_is_pair(doremir_processor_input_type(proc->elem))
           && doremir_type_is_pair(doremir_processor_output_type(proc->elem))
           && doremir_equal(
               doremir_type_get_pair_fst(doremir_processor_input_type(proc->elem)),
               doremir_type_get_pair_fst(doremir_processor_output_type(proc->elem))
           );
}

this_t doremir_processor_loop_create(processor_t proc1)
{
    this_t proc     = doremir_new(processor_loop_proc);
    proc->impl      = &loop_impl;

    proc->elem      = proc1;
    proc->elemImpl  = doremir_interface(doremir_processor_interface_i, proc->elem);
    assert(proc->elemImpl && "Must implement Processor");

    proc->bufType   = doremir_type_get_pair_fst(doremir_processor_input_type(proc->elem)); // TODO get fst

    if (type_check(NULL, proc)) {
        return proc;
    } else {
        assert(false && "Type error");
        // TODO
    }
}

void doremir_processor_loop_destroy(this_t proc)
{
    doremir_destroy(proc->elem);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

type_t loop_input_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return doremir_type_get_pair_fst(doremir_processor_input_type(proc->elem));
}

type_t loop_output_type(ptr_t a)
{
    this_t proc = (this_t) a;
    return doremir_type_get_pair_fst(doremir_processor_output_type(proc->elem));
}

size_t loop_buffer_size(frames_t frameSize, ptr_t a)
{
    this_t proc = (this_t) a;

    size_t inSize  = doremir_type_size_of(frameSize, loop_input_type(a));
    size_t outSize = doremir_type_size_of(frameSize, loop_output_type(a));
    size_t loopSize  = doremir_type_size_of(frameSize, proc->bufType);

    // FIXME should use buffer size of elements, not type size
    return size_max(inSize + loopSize, outSize + loopSize);
}

void loop_before(ptr_t a, info_t * info)
{
    this_t proc = (this_t) a;
    proc->elemImpl->before(proc->elem, info);
}

void loop_after(ptr_t a, info_t * info)
{
    this_t proc = (this_t) a;
    proc->elemImpl->after(proc->elem, info);
}

void loop_process(ptr_t a, info_t * info, samples_t samples)
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

    s = string_dappend(s, doremir_string_show(loop_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(loop_output_type(proc)));

    return s;
}

void loop_destroy(ptr_t a)
{
    doremir_processor_loop_destroy(a);
}

ptr_t loop_impl(doremir_id_t interface)
{
    static doremir_string_show_t loop_show_impl = { loop_show };
    static doremir_destroy_t loop_destroy_impl = { loop_destroy };
    static doremir_processor_interface_t loop_processor_interface_impl = {
        loop_before, loop_process, loop_after,
        loop_input_type, loop_output_type, loop_buffer_size
    };

    switch (interface) {
        case doremir_string_show_i:
            return &loop_show_impl;

        case doremir_destroy_i:
            return &loop_destroy_impl;

        case doremir_processor_interface_i:
            return &loop_processor_interface_impl;

        default:
            return NULL;
    }
}