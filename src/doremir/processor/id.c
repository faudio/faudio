
#import <doremir/processor/id.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_id_proc_t
{
    impl_t          impl;                       // Dispatcher
    type_t          type;                       // Type
};

typedef doremir_processor_id_proc_t         this_proc_t;
typedef doremir_processor_samples_t         samples_t;
typedef doremir_processor_info_t            info_t;

doremir_ptr_t id_impl(doremir_id_t interface);

this_proc_t doremir_processor_id_create(type_t type)
{
    this_proc_t proc  = doremir_new(processor_id_proc);
    proc->impl        = &id_impl;

    proc->type  = type;
    return proc;
}

void doremir_processor_id_destroy(this_proc_t proc)
{
    doremir_destroy(proc->type);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

doremir_type_t id_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return proc->type;
}

doremir_type_t id_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return proc->type;
}

size_t id_buffer_size(doremir_ptr_t a)
{
    // TODO
}

void id_before(doremir_ptr_t a, info_t *info)
{
    // nothing
}

void id_after(doremir_ptr_t a, info_t *info)
{
    // nothing
}

void id_process(ptr_t a, info_t *info, samples_t samples)
{
    // TODO
}

// --------------------------------------------------------------------------------

string_t id_show(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    string_t s = string("");

    s = string_dappend(s, doremir_string_show(proc->type));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(proc->type));

    return s;
}

void id_destroy(doremir_ptr_t a)
{
    doremir_processor_id_destroy(a);
}

doremir_ptr_t id_impl(doremir_id_t interface)
{
    static doremir_string_show_t id_show_impl = { id_show };
    static doremir_destroy_t id_destroy_impl = { id_destroy };
    static doremir_processor_interface_t id_processor_interface_impl =
    {
        id_before, id_process, id_after,
        id_input_type, id_output_type
    };

    switch (interface)
        {
        case doremir_string_show_i:
            return &id_show_impl;

        case doremir_destroy_i:
            return &id_destroy_impl;

        case doremir_processor_interface_i:
            return &id_processor_interface_impl;

        default:
            return NULL;
        }
}