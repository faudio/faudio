
#import <doremir/processor/loop.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_loop_proc_t
{
    impl_t              impl;           // Dispatcher

    proc_t              elem[1];
    proc_interface_t   *elemImpl[1];    // Fast pointer to the elements' processor implementation
};

typedef doremir_processor_loop_proc_t       this_proc_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

doremir_ptr_t loop_impl(doremir_id_t interface);

inline static 
bool check_type(string_t *msg, this_proc_t proc)
{
    if (msg)
        {
            *msg = string("Both input and output must be pair types");
        }

    return doremir_type_is_pair(doremir_processor_input_type(proc->elem[0]))
           && doremir_type_is_pair(doremir_processor_output_type(proc->elem[0]));
}

this_proc_t doremir_processor_loop_create(processor_t proc1)
{
    this_proc_t proc  = doremir_new(processor_loop_proc);
    proc->impl = &loop_impl;

    proc->elem[0]      = proc1;
    proc->elemImpl[0]  = doremir_interface(doremir_processor_interface_i, proc->elem[0]);

    if (check_type(NULL, proc))
        {
            return proc;
        }
    else
        {
            assert(false && "Type error");
            // TODO
        }
}

void doremir_processor_loop_destroy(this_proc_t proc)
{
    doremir_destroy(proc->elem[0]);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

void loop_before(doremir_ptr_t a, info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void loop_after(doremir_ptr_t a, info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void loop_process(ptr_t a, info_t *info, samples_t input, samples_t output)
{
    // TODO
    assert(false && "Missing");
}

doremir_type_t loop_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return doremir_type_get_pair_fst(doremir_processor_input_type(proc->elem[0]));
}

doremir_type_t loop_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return doremir_type_get_pair_fst(doremir_processor_output_type(proc->elem[0]));
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