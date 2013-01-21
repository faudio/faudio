
#import <doremir/processor/seq.h>
#import <doremir/string.h>
#import <doremir/util.h>

struct _doremir_processor_seq_proc_t
{
    impl_t              impl;           // Dispatcher

    proc_t              elem[2];        // Elements
    proc_interface_t   *elemImpl[2];    // Fast pointer to the elements' processor implementation

    type_t              midType;        // Buffer to transfer data between elements
    size_t              midSize;
    void               *midBuffer;
};

typedef doremir_processor_seq_proc_t    this_proc_t;
typedef doremir_processor_samples_t     samples_t;
typedef doremir_processor_info_t        info_t;

doremir_ptr_t seq_impl(doremir_id_t interface);

inline static 
bool check_type(string_t *msg, this_proc_t proc)
{
    if (msg)
        {
            *msg = string("Input type must equal output type");
        }

    return doremir_equal(
               doremir_processor_output_type(proc->elem[0]),
               doremir_processor_input_type(proc->elem[1])
           );
}

this_proc_t doremir_processor_seq_create(processor_t proc1, processor_t proc2)
{
    this_proc_t proc    = doremir_new(processor_seq_proc);
    proc->impl          = &seq_impl;

    proc->elem[0]       = proc1;
    proc->elem[1]       = proc2;
    proc->elemImpl[0]   = doremir_interface(doremir_processor_interface_i, proc->elem[0]);
    proc->elemImpl[1]   = doremir_interface(doremir_processor_interface_i, proc->elem[1]);

    proc->midType       = doremir_processor_output_type(proc->elem[0]);

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

void doremir_processor_seq_destroy(this_proc_t proc)
{
    doremir_destroy(proc->elem[0]);
    doremir_destroy(proc->elem[1]);
    doremir_delete(proc);
}

// --------------------------------------------------------------------------------

void seq_before(doremir_ptr_t a, info_t *info)
{
    this_proc_t proc = (this_proc_t) a;

    // Run subprocessors
    proc->elemImpl[0]->before(proc->elem[0], info);
    proc->elemImpl[1]->before(proc->elem[1], info);

    // Allocate mid buffer
    proc->midSize = doremir_type_size_of(info->frame_size, proc->midType);
    proc->midBuffer = malloc(proc->midSize);
    assert(proc->midBuffer && "malloc failed");
}

void seq_after(doremir_ptr_t a, info_t *info)
{
    this_proc_t proc = (this_proc_t) a;
    proc->elemImpl[0]->after(proc->elem[0], info);
    proc->elemImpl[1]->after(proc->elem[1], info);

    // Free mid buffer
    free(proc->midBuffer);
}

void seq_process(ptr_t a, info_t *info, samples_t input, samples_t output)
{
    this_proc_t proc = (this_proc_t) a;
    proc->elemImpl[0]->process(proc->elem[0], info, input, proc->midBuffer);
    proc->elemImpl[1]->process(proc->elem[1], info, proc->midBuffer, output);
}

doremir_type_t seq_input_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return doremir_processor_input_type(proc->elem[0]);
}

doremir_type_t seq_output_type(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    return doremir_processor_output_type(proc->elem[1]);
}

// --------------------------------------------------------------------------------

string_t seq_show(doremir_ptr_t a)
{
    this_proc_t proc = (this_proc_t) a;
    string_t s = string("");
    
    s = string_dappend(s, doremir_string_show(seq_input_type(proc)));
    s = string_dappend(s, string(" ~> "));
    s = string_dappend(s, doremir_string_show(seq_output_type(proc)));
    
    return s;
}

void seq_destroy(doremir_ptr_t a)
{
    doremir_processor_seq_destroy(a);
}

doremir_ptr_t seq_impl(doremir_id_t interface)
{
    static doremir_string_show_t seq_show_impl = { seq_show };
    static doremir_destroy_t seq_destroy_impl = { seq_destroy };
    static doremir_processor_interface_t seq_processor_interface_impl =
    {
        seq_before, seq_process, seq_after,
        seq_input_type, seq_output_type
    };

    switch (interface)
        {
        case doremir_string_show_i:
            return &seq_show_impl;

        case doremir_destroy_i:
            return &seq_destroy_impl;

        case doremir_processor_interface_i:
            return &seq_processor_interface_impl;

        default:
            return NULL;
        }
}