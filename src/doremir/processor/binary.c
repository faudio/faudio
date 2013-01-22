
#import <doremir/processor/binary.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_binary_proc_t
{
    impl_t          impl;                           // Dispatcher
    type_t          input_type[2], output_type;     // Types

    binary_t        function;                       // Lifted function and closure
    ptr_t           data;
};

typedef doremir_processor_binary_proc_t     this_proc_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

doremir_ptr_t binary_impl(doremir_id_t interface);

doremir_processor_binary_proc_t
doremir_processor_binary_create(doremir_type_t   type1,
                                doremir_type_t   type2,
                                doremir_type_t   type3,
                                doremir_binary_t function,
                                doremir_ptr_t    data)
{
    this_proc_t proc        = doremir_new(processor_binary_proc);
    proc->impl              = &binary_impl;

    proc->input_type[0]     = type1;
    proc->input_type[1]     = type2;
    proc->output_type       = type3;

    proc->function          = function;
    proc->data              = data;

    return proc;
}

void doremir_processor_binary_destroy(doremir_processor_binary_proc_t proc)
{
    doremir_destroy(proc->input_type);
    doremir_destroy(proc->output_type);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

doremir_type_t binary_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (doremir_processor_binary_proc_t) a;
    return doremir_type_pair(proc->input_type[0], proc->input_type[1]);
}

doremir_type_t binary_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (doremir_processor_binary_proc_t) a;
    return proc->output_type;
}

void binary_before(doremir_ptr_t a, info_t *info)
{
    // nothing
}

void binary_after(doremir_ptr_t a, info_t *info)
{
    // nothing
}

void binary_process(ptr_t a, info_t *info, samples_t input, samples_t output)
{
    // TODO
}

// --------------------------------------------------------------------------------

string_t binary_show(doremir_ptr_t a)
{
    this_proc_t proc = (doremir_processor_binary_proc_t) a;
    string_t s = string("");

    s = string_dappend(s, string("("));
    s = string_dappend(s, doremir_string_show(proc->input_type[0]));
    s = string_dappend(s, string(","));
    s = string_dappend(s, doremir_string_show(proc->input_type[1]));
    s = string_dappend(s, string(") ~> "));
    s = string_dappend(s, doremir_string_show(proc->output_type));

    return s;
}

void binary_destroy(doremir_ptr_t a)
{
    doremir_processor_binary_destroy(a);
}

doremir_ptr_t binary_impl(doremir_id_t interface)
{
    static doremir_string_show_t binary_show_impl = { binary_show };
    static doremir_destroy_t binary_destroy_impl = { binary_destroy };
    static doremir_processor_interface_t binary_processor_interface_impl =
    {
        binary_before, binary_process, binary_after,
        binary_input_type, binary_output_type
    };

    switch (interface)
        {
        case doremir_string_show_i:
            return &binary_show_impl;

        case doremir_destroy_i:
            return &binary_destroy_impl;

        case doremir_processor_interface_i:
            return &binary_processor_interface_impl;

        default:
            return NULL;
        }
}