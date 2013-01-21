
#import <doremir/processor/unary.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_unary_proc_t
{
    impl_t          impl;                       // Dispatcher
    type_t          input_type, output_type;    // Types
    
    unary_t         function;                   // Wrapped func
    ptr_t           data;
};

typedef doremir_processor_unary_proc_t this_proc_t;

doremir_ptr_t unary_impl(doremir_id_t interface);

this_proc_t
doremir_processor_unary_create(doremir_type_t  type1,
                               doremir_type_t  type2,
                               doremir_unary_t function,
                               doremir_ptr_t   data)
{
    this_proc_t proc  = doremir_new(processor_unary_proc);
    proc->impl        = &unary_impl;
    
    proc->input_type  = type1;
    proc->output_type = type2;
    
    proc->function    = function;
    proc->data        = data;
    
    return proc;
}

void
doremir_processor_unary_destroy(this_proc_t proc)
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

#define UNARY_PROCESSOR(A,B) \
    static inline \
    void unary_proc_##A##_##B \
    (int count, this_proc_t proc, ptr_t input, ptr_t output) \
    { \
        typedef A input_t; \
        typedef B output_t; \
        typedef output_t(func_t)(ptr_t, input_t); \
        \
        input_t*  raw_input     = (input_t*)  input; \
        output_t* raw_output    = (output_t*) output; \
        func_t*   raw_proc      = (func_t*)   proc->function; \
        \
        for(int i = 0; i < count; ++i) \
            raw_output[i] = raw_proc(proc->data, raw_input[i]); \
    }

UNARY_PROCESSOR(uint8_t,  uint8_t);
UNARY_PROCESSOR(uint16_t, uint16_t);
UNARY_PROCESSOR(uint32_t, uint32_t);
UNARY_PROCESSOR(uint64_t, uint64_t);
UNARY_PROCESSOR(float,    float);
UNARY_PROCESSOR(double,   double);
UNARY_PROCESSOR(ptr_t,    ptr_t);

void unary_process(doremir_ptr_t proc,
                   doremir_processor_info_t *info,
                   doremir_processor_samples_t samples)
{
    // unary_proc_uint8_t_uint8_t(1, proc2, samples, samples);
    // TODO
}

doremir_type_t unary_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return proc->input_type;
}

doremir_type_t unary_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return proc->output_type;
}

// --------------------------------------------------------------------------------

string_t unary_show(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    string_t s = string("");
    s = string_dappend(s, doremir_string_show(proc->input_type));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(proc->output_type));
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
    static doremir_processor_interface_t unary_processor_interface_impl =
    {
        unary_before, unary_process, unary_after,
        unary_input_type, unary_output_type
    };

    switch (interface)
        {
        case doremir_string_show_i:
            return &unary_show_impl;

        case doremir_destroy_i:
            return &unary_destroy_impl;

        case doremir_processor_interface_i:
            return &unary_processor_interface_impl;

        default:
            return NULL;
        }
}