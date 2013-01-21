
#import <doremir/processor/loop.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_loop_proc_t
{
    impl_t              impl;       // Dispatcher
    processor_t         elem;
};

typedef doremir_processor_loop_proc_t this_proc_t;

doremir_ptr_t loop_impl(doremir_id_t interface);

this_proc_t doremir_processor_loop_create(processor_t proc1)
{
    this_proc_t proc  = doremir_new(processor_loop_proc);
    proc->elem = proc1;
    return proc;
}

void
doremir_processor_loop_destroy(this_proc_t proc)
{
    doremir_destroy(proc->elem);
    doremir_destroy(proc->elem);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

void loop_before(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void loop_after(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void loop_process(doremir_ptr_t proc, doremir_processor_info_t *info, doremir_processor_samples_t samples)
{
    // this_proc_t proc2 = (this_proc_t) proc;
    // TODO
    assert(false && "Missing");
}

doremir_type_t loop_input_type(doremir_ptr_t a)
{
    // TODO
    assert(false && "Missing");
}

doremir_type_t loop_output_type(doremir_ptr_t a)
{
    // TODO
    assert(false && "Missing");
}

// --------------------------------------------------------------------------------

string_t loop_show(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    string_t s = string("");
    s = string_dappend(s, doremir_string_show(loop_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(loop_output_type(proc)));
    return s;
}

void loop_destroy(doremir_ptr_t a)
{
    doremir_processor_loop_destroy(a);
}

// TODO make copyable?
doremir_ptr_t loop_impl(doremir_id_t interface)
{
    static doremir_string_show_t loop_show_impl = { loop_show };
    static doremir_destroy_t loop_destroy_impl = { loop_destroy };
    static doremir_processor_interface_t loop_processor_interface_impl =
    {
        loop_before, loop_process, loop_after,
        loop_input_type, loop_output_type
    };

    switch (interface)
        {
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