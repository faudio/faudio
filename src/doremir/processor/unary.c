
#import <doremir/processor/unary.h>
#import <doremir/util.h>

struct _doremir_processor_unary_proc_t {
        impl_t impl;                /* Interface dispatcher. */

        type_t input_type;
        type_t output_type;

        unary_t function;
};
            
typedef doremir_processor_unary_proc_t this_proc_t;

doremir_ptr_t unary_impl(doremir_id_t interface);

doremir_processor_unary_proc_t 
doremir_processor_unary_create(doremir_type_t type1, 
                               doremir_type_t type2,
                               doremir_unary_t function)
{
    this_proc_t proc = doremir_new(processor_unary_proc);
    proc->impl = &unary_impl;
    proc->input_type = type1;
    proc->output_type = type2;
    proc->function = function;
    return proc;
}

void 
doremir_processor_unary_destroy(doremir_processor_unary_proc_t proc)
{
    doremir_destroy(proc->input_type);
    doremir_destroy(proc->output_type);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------


string_t unary_show(doremir_ptr_t a)
{
    doremir_processor_unary_proc_t unary = (doremir_processor_unary_proc_t) a;
    string_t s = string("");
    return s;
}

void unary_destroy(doremir_ptr_t a)
{
    doremir_processor_unary_destroy(a);
}

void before(doremir_ptr_t a, doremir_processor_info_t *info)
{
}


doremir_processor_samples_t process(doremir_ptr_t a,
                                    doremir_processor_info_t * info,
                                    doremir_processor_samples_t input)
{
}


void after(doremir_ptr_t a, doremir_processor_info_t *info)
{
}


doremir_type_t input_type()
{
}

doremir_type_t output_type()
{
}


doremir_ptr_t unary_impl(doremir_id_t interface)
{
    static doremir_string_show_t unary_show_impl = { unary_show };
    static doremir_destroy_t unary_destroy_impl = { unary_destroy };

    switch (interface)
    {
    case doremir_string_show_i:
        return &unary_show_impl;

    case doremir_destroy_i:
        return &unary_destroy_impl;

    default:
        return NULL;
    }
}       