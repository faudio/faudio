
## Boxed types

\anchor BoxedTypes

TODO intro

The boxing functions can be used to convert primitive values to \ref doremir_ptr_t and vice versa. 
They are the preferred way of storing primitive types in a collection. The box and unbox functions
are implemented without heap allocation whenever possible, but types that are to large to fit into a 
pointer are always heap-allocated.

The boxed values have the same life cycle as collections, that is: create, use, destroy (once).
However, passing a boxed value to a generic function does *not* result in the boxed value being
destroyed. Wrapped values stored in collections are owned by the collection and will be destroyed
when the collection is destroyed. This means that boxed values can not be shared without explicit
copying (the same holds for nested collections).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_t xs = doremir_list_empty();
{
    xs = doremir_list_consd(d(1.4142135623730951), xs);
    xs = doremir_list_consd(d(3.141592653589793), xs);
    double s = td(doremir_list_sum());
}
doremir_list_destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The boxed values implement \ref doremir_equal_t, \ref doremir_order_t, \ref doremir_number_t, \ref
doremir_copy_t, \ref doremir_destroy_t and \ref doremir_dynamic_t.

The standard version of the wrapper functions are declared in the \ref doremir.h header, but the shorter
aliases in \ref doremir/util.h are usually more convenient to use.

### Defined in doremir/util.h

|        | Wrap           | Unwrap         
|--------|----------------| -------------------
| bool   | \ref b         | \ref tb        
| int8   | \ref i8        | \ref ti8       
| int16  | \ref i16       | \ref ti16      
| int32  | \ref i32       | \ref ti32      
| int32  | \ref i64       | \ref ti64      
| double | \ref d         | \ref td        

### Defined in doremir.h

|        | Wrap                     | Unwrap                  
|--------|--------------------------| ------------------------
| bool   | \ref doremir_from_bool   | \ref doremir_to_bool    
| int8   | \ref doremir_from_int8   | \ref doremir_to_int8    
| int16  | \ref doremir_from_int16  | \ref doremir_to_int16   
| int32  | \ref doremir_from_int32  | \ref doremir_to_int32   
| double | \ref doremir_from_double | \ref doremir_to_double  

