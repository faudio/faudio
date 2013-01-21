
#import <doremir/processor/binary.h>
#import <doremir/util.h>

struct _doremir_processor_binary_proc_t {
        impl_t impl;    /* Interface dispatcher. */
    
};

doremir_processor_binary_proc_t 
doremir_processor_binary_create(doremir_type_t   type1,
                                doremir_type_t   type2,
                                doremir_type_t   type3,
                                doremir_binary_t function,
                                doremir_ptr_t    data)
{
    
}

void 
doremir_processor_binary_destroy(doremir_processor_binary_proc_t proc)
{
    
}
