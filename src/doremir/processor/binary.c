
#import <doremir/processor/binary.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_binary_proc_t
{
    impl_t          impl;                           // Dispatcher
    type_t          input_type[2], output_type;     // Types

    binary_t        function;                       // Wrapped func
    ptr_t           data;
};

typedef doremir_processor_binary_proc_t this_proc_t;

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
    proc->input_type[1]     = type1;
    proc->output_type       = type2;
    proc->function          = function;
    proc->data              = data;
    return proc;
}

void
doremir_processor_binary_destroy(doremir_processor_binary_proc_t proc)
{
    doremir_destroy(proc->input_type);
    doremir_destroy(proc->output_type);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

void binary_before(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // nothing
}

void binary_after(doremir_ptr_t a, doremir_processor_info_t *info)
{
    // nothing
}

/*
#define BINARY_PROCESSOR(A,B) \
    static inline \
    void binary_proc_##A##_##B \
    (int count, this_proc_t proc, buffer_t input, buffer_t output) \
    { \
        typedef A input_t; \
        typedef B output_t; \
        typedef output_t(func_t)(ptr_t, input_t); \
        \
        input_t*  raw_input     = (input_t*)  doremir_buffer_unsafe_address(input); \
        output_t* raw_output    = (output_t*) doremir_buffer_unsafe_address(output); \
        func_t*   raw_proc      = (func_t*)   proc->function; \
        \
        for(int i = 0; i < count; ++i) \
            raw_output[i] = raw_proc(proc->data, raw_input[i]); \
    }

BINARY_PROCESSOR(uint8_t, uint8_t);
BINARY_PROCESSOR(float,   float);
BINARY_PROCESSOR(double,  double);
*/

void binary_process(doremir_ptr_t proc,
                    doremir_processor_info_t *info,
                    doremir_processor_samples_t samples)
{
    // TODO
}

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