
\anchor Collections

The Audio Engine include a set of basic collections, including \ref DoremirPair, \ref DoremirList, \ref DoremirMap and \ref DoremirSet. These collections all obey the following rules:

* They are *immutable*
* They are *polymorphic*
* They have *single-ownership* semantics

### Immutable

The collections used by the Audio Engine can not change. Functions that such as \ref doremir_list_map
return new collections instead.

### Polymorphic

The collections can store reference types (as \ref doremir_ptr_t) and primitive types by using the
\ref WrapFunctions "wrap functions".

                                                                   

### Single-ownership

The Audio Engine use single-ownership semantics for all of its reference types. Each collection provides a set of construct, copy and destruct function, and each collection must be destructed exactly once. The correct usage pattern is to call the destructor whenever the variable holding the collection goes out of scope. It is good practice to introduce an extra block to mark out the scope of the collection variable.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    doremir_list_t xs = doremir_list(1, 2, 3);
    
    ... // xs can be used here
    
    doremir_destroy(xs);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The reference types can be shared as long as the original reference is in scope.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    doremir_list_t xs = doremir_list(1, 2, 3);
    
    int z = doremir_list_sum(xs); // xs passed "by reference"

    doremir_destroy(xs);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a reference type is required to persist beyond the original scope it can be copied or moved.
The \ref doremir_move function does nothing, it just serves as a mnemonic to mark out that
ownership is being transfered.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    doremir_list_t xs = doremir_list(1, 2, 3);
    
    doremir_send(out1, doremir_copy(xs)); // xs passed "by copy"
    doremir_send(out1, doremir_copy(xs)); // xs passed "by copy"
    ...

    doremir_destroy(xs);
}

{
    doremir_list_t ys = doremir_list(1, 2, 3);
    
    doremir_send(out1, doremir_copy(ys)); // ys passed "by copy"
    doremir_send(out1, doremir_move(ys)); // ys passed "by value"

    // no destroy needed
}                                                   
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Functions operating on collections come in two variants: a non-destructive variant that simply construct a
new collection (the default), and a destructive variant which destructs the old collections while
constructing the new one. The destructive functions should be used whenever a variable is updated locally.

Note that the destructive functions *invalidates* the original collection rather than *mutating* it.
Invalidating a collection does not affect copies.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    doremir_list_t xs;
    doremir_list_t ys;
    int i;

    xs = doremir_list();
    for (i = 0; i < 10; ++i)
        xs = doremir_list_consd(i, xs);
    
    ys = doremir_copy(xs);
    for (i = 10; i < 15; ++i)
        ys = doremir_list_consd(3, ys);
    
    // xs is [1..9]
    // ys is [1..14]

    doremir_destroy(ys);
    doremir_destroy(xs);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<!--
Functions that are unsymmetric in their construct/destruct calls becomes construct/destruct functions
themselves.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t doremir_list_single(doremir_ptr_t x)
{                                
    doremir_list_t xs = doremir_list();
    xs = doremir_list_consd(1, xs);
    return xs; 
}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-->
