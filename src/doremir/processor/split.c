
#import <doremir/processor/split.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_split_proc_t
{
    impl_t              impl;       // Dispatcher
    type_t              type;       // Type of input
};

typedef doremir_processor_split_proc_t this_proc_t;

doremir_ptr_t split_impl(doremir_id_t interface);

inline static bool check_type(string_t* msg, this_proc_t proc)
{   
    // Nothing to check
    return true;
}

this_proc_t doremir_processor_split_create(doremir_type_t type)
{
    this_proc_t proc  = doremir_new(processor_split_proc);
    proc->impl = &split_impl;
    
    proc->type = type;

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

void
doremir_processor_split_destroy(this_proc_t proc)
{
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

void split_before(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void split_after(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void split_process(doremir_ptr_t proc, doremir_processor_info_t *info, doremir_processor_samples_t samples)
{
    // TODO
    assert(false && "Missing");
}

doremir_type_t split_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return doremir_type_copy(proc->type);
}

doremir_type_t split_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return doremir_type_pair(proc->type, proc->type);
}

// --------------------------------------------------------------------------------

string_t split_show(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    string_t s = string("");
    s = string_dappend(s, doremir_string_show(split_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(split_output_type(proc)));
    return s;
}

void split_destroy(doremir_ptr_t a)
{
    doremir_processor_split_destroy(a);
}

doremir_ptr_t split_impl(doremir_id_t interface)
{
    static doremir_string_show_t split_show_impl = { split_show };
    static doremir_destroy_t split_destroy_impl = { split_destroy };
    static doremir_processor_interface_t split_processor_interface_impl =
    {
        split_before, split_process, split_after,
        split_input_type, split_output_type
    };

    switch (interface)
        {
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