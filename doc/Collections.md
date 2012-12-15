
## Collections

The Audio Engine include a set of basic collections, including \ref DoremirPair, \ref DoremirList, \ref DoremirMap and \ref DoremirSet. These collections all obey the following rules:

* They are *immutable*
* They are *polymorphic*
* They have *single-ownership* semantics

### Immutable

The collections used by the Audio Engine can not change. Functions that such as \ref doremir_list_map return new collections instead.

### Polymorphic

The collections can store reference types (as \ref doremir_ptr_t) and primitive types by using the
\ref Wrapping "wrap functions".

                                                                   

### Single-ownership

The Audio Engine use single-ownership semantics for all of its reference types. Each collection provides a set of construct, copy and destruct function, and each collection must be destructed exactly once. The correct usage pattern is to call the destructor whenever the variable holding the collection goes out of scope. It is good practice to introduce an extra block to mark out the scope of the collection variable.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_t xs = list(1, 2, 3);
{
    ...
}
destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The reference types can be shared as long as the original reference is in scope.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_t xs = list(1, 2, 3);
{
    int sum = sum(xs);
    ...
}
destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a reference type is required to persist beyond the original scope it can be copied or moved.
The \ref doremir_move function does nothing, it just serves as a mnemonic to mark out that
ownership is being transfered.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_list_t xs = doremir_list_from(1, 2, 3);
{
    send(out1, copy(xs));
    send(out1, copy(xs));
    ...
}
doremir_list_destroy(xs);

doremir_list_t ys = doremir_list_from(1, 2, 3);
{
    send(out1, copy(ys));
    send(out1, move(ys));
}                                                   
// no destroy needed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Functions operating on collections come in two variants: a non-destructive variant that simply construct a
new collection (the default), and a destructive variant which destructs the old collections while
constructing the new one. The destructive functions should be used whenever a variable is updated locally.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_t xs = list();
{
    xs = consd(1, xs);
    xs = consd(2, xs);
    xs = consd(3, xs);
}
destroy(xs);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions that are unsymmetric in their construct/destruct calls becomes construct/destruct functions
themselves.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_t doremir_list_single(doremir_ptr_t x)
{                                
    list_t xs = list();
    xs = consd(1, xs);

    // no destroy
    return xs; 
}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

