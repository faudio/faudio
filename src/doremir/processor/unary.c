
#import <doremir/processor/unary.h>
#import <doremir/string.h>
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
    this_proc_t proc  = doremir_new(processor_unary_proc);
    proc->impl        = &unary_impl;

    proc->input_type  = type1;
    proc->output_type = type2;

    proc->function    = function;
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

void unary_before(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // nothing
}

void unary_after(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // nothing
}

#define PROC(A,B) \
    static inline                                                                                 \
    void unary_proc_##A##_##B(this_proc_t proc, buffer_t input, buffer_t output)                  \
    {                                                                                             \
        typedef A this_input_t;                                                                   \
        typedef B this_output_t;                                                                  \
        typedef this_output_t(this_func_t)(this_input_t);                                         \
                                                                                                  \
        this_input_t*  raw_input     = (this_input_t*)  doremir_buffer_unsafe_address(input);     \
        this_output_t* raw_output    = (this_output_t*) doremir_buffer_unsafe_address(output);    \
        this_func_t*   proc_function = (this_func_t*)   proc->function;                           \
        raw_output[0] = proc_function(raw_input[0]);                                              \
    }
    
PROC(uint8_t,uint8_t);
PROC(double,double);

doremir_processor_samples_t unary_process(doremir_ptr_t proc,
                                         doremir_processor_info_t * info,
                                         doremir_processor_samples_t samples)
{
    this_proc_t proc2  = (this_proc_t) proc;
    buffer_t    input  = samples;
    buffer_t    output = samples;
    
    // TODO dispatch
    unary_proc_uint8_t_uint8_t(proc2, input, output);
    return output;
}

doremir_type_t unary_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (doremir_processor_unary_proc_t) a;
    return proc->input_type;
}

doremir_type_t unary_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (doremir_processor_unary_proc_t) a;
    return proc->output_type;
}

// --------------------------------------------------------------------------------

string_t unary_show(doremir_ptr_t a)
{
    this_proc_t proc = (doremir_processor_unary_proc_t) a;
    string_t s = string("");
    s = sdappend(s, doremir_string_show(proc->input_type));
    s = sdappend(s, string(" ~> "));
    s = sdappend(s, doremir_string_show(proc->output_type));
    return s;
}

void unary_destroy(doremir_ptr_t a)
{
    doremir_processor_unary_destroy(a);
}

doremir_ptr_t unary_impl(doremir_id_t interface)
{
    static doremir_string_show_t unary_show_impl = { unary_show };
    static doremir_destroy_t unary_destroy_impl = { unary_destroy };
    static doremir_processor_t unary_processor_impl = { 
        unary_before, unary_process, unary_after, 
        unary_input_type, unary_output_type 
    };

    switch (interface)
    {
    case doremir_string_show_i:
        return &unary_show_impl;

    case doremir_destroy_i:
        return &unary_destroy_impl;

    case doremir_processor_i:
        return &unary_processor_impl;

    default:
        return NULL;
    }
}       