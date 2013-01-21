
#import <doremir/processor/par.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_par_proc_t
{
    impl_t              impl;           // Dispatcher
    processor_t         elem[2];        // Elements
};

typedef doremir_processor_par_proc_t this_proc_t;

doremir_ptr_t par_impl(doremir_id_t interface);

inline static bool check_type(string_t* msg, this_proc_t proc)
{       
    // Nothing to check
    return true;
}

this_proc_t doremir_processor_par_create(processor_t proc1, processor_t proc2)
{
    this_proc_t proc  = doremir_new(processor_par_proc);
    proc->impl = &par_impl;
    proc->elem[0] = proc1;
    proc->elem[1] = proc2;

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
doremir_processor_par_destroy(this_proc_t proc)
{
    doremir_destroy(proc->elem[0]);
    doremir_destroy(proc->elem[0]);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

void par_before(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void par_after(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // TODO
    assert(false && "Missing");
}

void par_process(doremir_ptr_t proc, doremir_processor_info_t *info, doremir_processor_samples_t samples)
{
    // TODO
    assert(false && "Missing");
}

doremir_type_t par_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    type_t t0 = doremir_processor_input_type(proc->elem[0]);
    type_t t1 = doremir_processor_input_type(proc->elem[1]);
    return doremir_type_pair(t0,t1);
}

doremir_type_t par_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    type_t t0 = doremir_processor_output_type(proc->elem[0]);
    type_t t1 = doremir_processor_output_type(proc->elem[1]);
    return doremir_type_pair(t0,t1);
}

// --------------------------------------------------------------------------------

string_t par_show(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    string_t s = string("");
    s = string_dappend(s, doremir_string_show(par_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(par_output_type(proc)));
    return s;
}

void par_destroy(doremir_ptr_t a)
{
    doremir_processor_par_destroy(a);
}

doremir_ptr_t par_impl(doremir_id_t interface)
{
    static doremir_string_show_t par_show_impl = { par_show };
    static doremir_destroy_t par_destroy_impl = { par_destroy };
    static doremir_processor_interface_t par_processor_interface_impl =
    {
        par_before, par_process, par_after,
        par_input_type, par_output_type
    };

    switch (interface)
        {
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