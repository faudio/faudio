
# Collections {#Collections}

@anchor Collections

@note
    This is an advanced topic, not necessary to read for general usage. On the other hand, you *should* probably read
    it if you want to modify or extend the Audio Engine.

@tableofcontents

The Audio Engine include a set of container data structures, commonly known as collections. The
primary use of these types it to be used as messages, to be sent and received from audio
computations. For this reason the Audio Engine collections are *immutable* and *polymorphic*.

In contrast to container types in managed languages, the collection types used in the Audio Engine
are not shared, instead they have *single-ownership semantics*.

All collections implement the following [interfaces](@ref Interfaces):

* doremir_equal_t
* doremir_order_t
* doremir_copy_t
* doremir_destroy_t
* doremir_dynamic_t
* doremir_string_show_t


# Collection properties {#CollectionProperties}

## Immutable {#Immutable}

The collections used by the Audio Engine can not change. Functions that such as @ref
doremir_list_map return new collections instead.

Immutability allows collections to be passed between threads without synchronization issues.

## Polymorphic {#Polymorphic}

The collections store @ref doremir_ptr_t by default. Other reference types can be stored by casting.
Primitive types such as integers, floats and characters can be stored by using 
[boxed types](@ref BoxedTypes). 

It is not recommended to store integral types by casting to pointers.


## Single-ownership {#si}

The Audio Engine use single-ownership semantics for all of its reference types. Each collection
provides a set of construct, copy and destruct function, and each collection must be destructed
exactly once. 

# Memory management {#CollectionMemory}

The correct usage pattern is to call the destructor whenever the variable holding the
collection goes out of scope. It is good practice to introduce an extra block to mark out the scope
of the collection variable.

~~~~
{
    doremir_list_t xs = doremir_list(1, 2, 3);

    ... // xs can be used here

    doremir_destroy(xs);
}
~~~~

The reference types can be shared as long as the original reference is in scope.

~~~~
{
    doremir_list_t xs = doremir_list(1, 2, 3);

    int z = doremir_list_sum(xs); // xs passed "by reference"

    doremir_destroy(xs);
}
~~~~

If a reference type is required to persist beyond the original scope it can be copied or moved. The
@ref doremir_move function does nothing, it just serves as a mnemonic to mark out that ownership is
being transfered.

~~~~
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
~~~~


Functions operating on collections come in two variants: a non-destructive variant that simply
construct a new collection (the default), and a destructive variant which destructs the old
collections while constructing the new one. The destructive functions should be used whenever a
variable is updated locally.

Note that the destructive functions *invalidates* the original collection rather than *mutating*
it. Invalidating a collection does not affect copies.

~~~~
{
    doremir_list_t xs;
    doremir_list_t ys;
    int i;

    xs = doremir_list();                    // create list xs
    for (i = 0; i < 3; ++i)
        xs = doremir_list_consd(i, xs);     // modify xs

    ys = doremir_copy(xs);                  // copy xs to ys
    ys = doremir_list_consd(3, ys);         // modify ys

    // xs is [1,2,3]
    // ys is [1,2,3,4]

    doremir_destroy(ys);                    // destroy both lists
    doremir_destroy(xs);
}
~~~~


# Boxes {#BoxedTypes}

TODO intro

The boxed values implement the standard collection interfaces, as well as @ref doremir_number_t.

|        | Box            | Unbox         
|--------|----------------| -------------------
| bool   | @ref b         | @ref tb        
| int8   | @ref i8        | @ref ti8       
| int16  | @ref i16       | @ref ti16      
| int32  | @ref i32       | @ref ti32      
| int32  | @ref i64       | @ref ti64      
| double | @ref d         | @ref td        

The boxing functions can be used to convert primitive values to @ref doremir_ptr_t and vice versa. 
They are the preferred way of storing primitive types in a collection. The box and unbox functions
are implemented without heap allocation whenever possible, but types that are to large to fit into a 
pointer are always heap-allocated.

                                        
~~~~
list_t xs = doremir_list(d(1.4142135623730951), d(3.141592653589793));    
double s = td(doremir_list_dfold(doremir_add, d(0), xs));
~~~~



