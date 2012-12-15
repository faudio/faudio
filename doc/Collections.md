
Collections
===============

The Audio Engine include a set of basic collections, including \ref DoremirPair, \ref DoremirList, \ref DoremirMap and \ref DoremirSet. These collections all obey the following rules:

* They are *immutable*
* They are *polymorphic*
* They have *single-ownership* semantics

### Immutable

The collections used by the Audio Engine can not change. Functions that such as \ref doremir_list_map return new collections instead.

### Polymorphic

The collections can store reference types (as \ref doremir_ptr_t) and primitive types by using the \ref WrapperFunctions "wrapper functions".


### Single-ownership

The Audio Engine use single-ownership semantics for all of its reference types. Each collection provides a set of construct, copy and destruct function, and each collection must be destructed exactly once. The correct usage pattern is to call the destructor whenever the variable holding the collection goes out of scope. It is good practice to introduce an extra block to mark out the scope of the collection variable.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t xs = doremir_list_from(1, 2, 3);
{
    ...
}
doremir_list_destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The reference types can be shared as long as the original reference is in scope.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t xs = doremir_list_from(1, 2, 3);
{
    int sum = doremir_list_sum(xs);
    ...
}
doremir_list_destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a reference type is required to persist beyond the original scope it can be copied or moved.
The \ref doremir_move function does nothing, it just serves as a mnemonic to mark out that
ownership is being transfered.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t xs = doremir_list_from(1, 2, 3);
{
    doremir_dispatcher_send(out1, doremir_copy(xs));
    doremir_dispatcher_send(out1, doremir_copy(xs));
    ...
}
doremir_list_destroy(xs);

doremir_list_t ys = doremir_list_from(1, 2, 3);
{
    doremir_dispatcher_send(out1, doremir_copy(ys));
    doremir_dispatcher_send(out1, doremir_move(ys));
}                                                   
// no destroy needed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Functions operating on collections come in two variants: a non-destructive variant that simply construct a
new collection (the default), and a destructive variant which destructs the old collections while
constructing the new one. The destructive functions should be used whenever a variable is updated locally.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t xs = doremir_list_empty();
{
    xs = doremir_list_consd(1, xs);
    xs = doremir_list_consd(2, xs);
    xs = doremir_list_consd(3, xs);
}
doremir_list_destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions that are unsymmetric in their construct/destruct calls becomes construct/destruct functions
themselves.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t doremir_list_single(doremir_ptr_t x)
{                                
    doremir_list_t xs = doremir_list_empty();
    xs = doremir_list_consd(1, xs);

    // no destroy
    return xs; 
}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Collections and interfaces

Many functions operating on collections require the contained type to implement a certain interface. For
example \ref doremir_list_sort requires the contents of the given list to implenent doremir_ord_t.


\anchor WrapperFunctions

### Wrapper functions

The wrapper functions can be used to convert primitive values to \ref doremir_ptr_t and back, and is the preferred
way of storing primitive types in a collection. The standard version of the wrapper functions are declared in the \ref Doremir module,
but the shorter aliases in \ref doremir/util.h are usually more conventient.

Type   | From pointer                          | To pointer
-------| --------------------------------------|--------------------
bool   | \ref doremir_to_bool, \ref d_bool     | \ref doremir_from_bool, \ref d_fbool
int8   | \ref doremir_to_int8, \ref d_int8       | \ref doremir_from_int8, \ref d_int8
int16  | \ref doremir_to_int16, \ref d_int16     | \ref doremir_from_int16, \ref d_int16
int32  | \ref doremir_to_int32, \ref d_int32     | \ref doremir_from_int32, \ref d_fint32
float  | \ref doremir_to_float, \ref d_float     | \ref doremir_from_float, \ref d_ffloat
double | \ref doremir_to_double, \ref d_double   | \ref doremir_from_double, \ref d_fdouble

The wrapping pointers have the same life cycle collections, that is: create once, use, destroy once. Typically they should be used in conjunction with collection functions.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t xs = doremir_list_empty();
{
    xs = doremir_list_consd(d_double(1.4142135623730951), xs);
    xs = doremir_list_consd(d_double(3.141592653589793), xs);
    double s = d_fdouble(doremir_list_sum());
}
doremir_list_destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The wrapping pointers implement the standard interfaces (\ref doremir_eq_t, \ref doremir_ord_t and \ref
doremir_num_t) and can be used with \ref doremir_get_interface.

### Dynamic collections

Collections containing only primitives or references implementing \ref doremir_get_interface can be used
with the \ref DoremirDynamic functions.


