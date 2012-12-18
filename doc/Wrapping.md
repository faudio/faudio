
\anchor WrapFunctions

The wrap functions can be used to convert primitive values to \ref doremir_ptr_t and vice versa. 
They are the preferred way of storing primitive types in a collection. 

The wrapped values have the same life cycle collections, that is: create once, use, destroy once.
Typically they should be used in conjunction with collection functions. The wrap and unwrap functions
are implemented without heap allocation whenever possible, but some types (notably double) 
are to large to fit into a pointer and are always heap-allocated.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_t xs = doremir_list_empty();
{
    xs = doremir_list_consd(d(1.4142135623730951), xs);
    xs = doremir_list_consd(d(3.141592653589793), xs);
    double s = td(doremir_list_sum());
}
doremir_list_destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The wrapped values implement \ref doremir_equal_t, \ref doremir_order_t, \ref doremir_number_t, \ref
doremir_copy_t, \ref doremir_destroy_t and \ref doremir_dynamic_t.

The standard version of the wrapper functions are declared in the \ref doremir.h header, but the shorter
aliases in \ref doremir/util.h are usually more convenient to use.

### Defined in doremir/util.h

|        | doremir_ptr_t | doremir_ptr_t
|--------| --------------|--------------------
| bool   | \ref tb        | \ref b
| int8   | \ref ti8       | \ref i8
| int16  | \ref ti16      | \ref i16
| int32  | \ref ti32      | \ref i32
| int32  | \ref ti64      | \ref i64
| double | \ref td        | \ref d

### Defined in doremir.h

|        | doremir_ptr_t           | doremir_ptr_t
|--------| ------------------------|--------------------
| bool   | \ref doremir_to_bool    | \ref doremir_from_bool
| int8   | \ref doremir_to_int8    | \ref doremir_from_int8
| int16  | \ref doremir_to_int16   | \ref doremir_from_int16
| int32  | \ref doremir_to_int32   | \ref doremir_from_int32
| double | \ref doremir_to_double  | \ref doremir_from_double

