
# Data structures {#DataStructures}

@anchor DataStructures
@tableofcontents

@note
    This page is under construction.

The Audio Engine include a set of general purpose
[persistent&nbsp;data&nbsp;structures][persistent], which are primarily used for
message passing between the audio thread and the main thread. The fact that the
data structures are persistent eliminate many of the problems commonly associated
with multi-threaded programming and promotes a functional style.

The data structures in the Audio Engine are somewhat different from the structures
found in most languages, in that they have single-ownership semantics. This
eliminates the need for a garbage collector while still allowing a high-level 
interface.

Note that there is no interface capturing the notion of a data structure: they are
simply reference types obeying the conventions described below. However, all data
structures support generic [equality](@ref doremir_equal_t) or 
[ordering](@ref doremir_order_t), [copying](@ref doremir_copy_t) and 
[destruction](@ref doremir_destroy_t).


# Overview {#Overview}

The core data structures are:

Type                           | Semantics
-------------------------------|------------------------------------------------------
[Pair](@ref DoremirPair)       | An ordered pair
[List](@ref DoremirList)       | An ordered sequence
[Set](@ref DoremirSet)         | An ordered set
[Map](@ref DoremirMap)         | A set of ordered pairs
[Graph](@ref DoremirGraph)     | A labeled, directed graph

There is also a set of *mutable* data structures not included in this table. These
are used internally in the Audio Engine and need rarely be accessed by the user. For
completeness, they are:

Type                                        | Semantics
--------------------------------------------|------------------------------------------------------
[Priority queue](@ref DoremirPriorityQueue) | A first-in, ordered out priority queue



# Using data structures {#Conventions}

## Literals, read and show {#Literals}

All data structures have literals defined in the [utility&nbsp;headers][util].
These always evaluate to a newly created instance of the data structure.

- `pair(i32(1), i32(2))`
- `list(i32(1), i32(2), i32(3))`
- `set(i32(1), i32(2), i32(3))`
- `map(string("foo"),i32(1),string("bar"),i32(2))`
- `graph(i32(1),i32(2),edge(i32(1),i32(2),i32(3)))`

- `(1,2)`
- `[1,2,3]`
- `{1,2,3}`
- `{foo:1,bar:2}`
- `({1,2,3},{((1,2),"foo"),((1,3):"bar")})`

@warning
    Beware not to use a structure literal on an integer, you must use 
    [value references](@ref ValueReferences).
    

## Thread safety {#ThreadSafety}

All operations on data structures are thread-safe except creation, copying and
destruction, which are subject to the following restrictions:

* If a data structure *a* is created in thread *t* and used in thread *u*,
  an ordering must be established between the creation function returning
  in *t* and subsequent use of *a* in *u*.

* If a data structure *a* is used in thread *t* and destroyed in thread *u*,
  an ordering must be established between the last use of *a* in *t* and 
  the destructive function being applied in *u*.

Note that copying is considered both *usage* (of the old value) and a *creation*
(of the copy). Similarly, a function such as @ref doremir_list_dcons is both
destructive function (of the old value) and a creation function (of the new value).


## Polymorphic structures {#Polymorphic}

The use of the polymorphic data structures such as lists are somewhat hampered by
the lack of polymorphism in the C language. This means that there is no way to
create types by type-level application, so all lists are of the same type,
regardless of what type they contain. This is not a problem in a dynamic language
such as Lisp, where the type of a value is always known at runtime; however it is a
problem in languages such as C, as passing the wrong type to a function will likely
result in memory corruption and a program crash.

All data structures can store all reference types, including
[value&nbsp;references](@ref ValueReferences). The user must rely on guarantees
outside the scope of the compiler to assure that the extracted elements are of the
right type. There are several ways to do this:

* Assure that the structure contains a specific type
    * For example, many functions in the Audio Engine API return pairs and lists, their
      documentation clearly stating what type of elements the list will contain.
* Assure that the structure contains a generic type
    * For example, a function may require a set of values implementing [Show](@ref doremir_string_show_t).

In some cases, it does not matter what type a data structure contains, as the
elements are not going to be inspected. For example, @ref doremir_list_reverse can
receive a list of any type, as it operates purely on the structure of the list and
does not need to use its values.


## Creation, copying and destruction {#CreateCopyDestroy}

TODO destructive functions

# Value references {#ValueReferences}

Value types can be converted to reference types using a set of conversion
functions. These functions take a non reference type and return a so called *value
reference*. This reference can subsequently be destroyed and its value extracted.

A value reference is a proper reference that can be safely stored in a data
structure. Like other data structures, value references may be created and
destroyed from any thread and have single ownership semantics. Value references are
in fact tiny data structures containing a single element. They support all normal
data structure operations including copying, destruction and

It is not specified exactly how value references are implemented; however the
reference representation of a value typically have a different bit pattern from the
represented value. It is not safe to cast a value of a primitive type, such as an
integer, to a pointer and treat the resulting address as a value reference.
Conversely, it is not safe to treat a value reference as a pointer by dereferencing
it, or doing pointer arithmetic. It is guaranteed that the value references will
never overlap with real references. 

## Creating a value reference {#CreatingValueReference}

Value references are created by the following functions:

* [fromBool](@ref doremir_from_bool)
* [fromInt8](@ref doremir_from_bool)
* [fromInt16](@ref doremir_from_bool)
* [fromInt32](@ref doremir_from_bool)
* [fromInt64](@ref doremir_from_bool)
* [fromFloat](@ref doremir_from_bool)
* [fromDouble](@ref doremir_from_bool)


## Checking the type of a value reference {#CheckingTypeValueReference}

The [isRef](@ref doremir_from_bool) and [isValue](@ref doremir_from_bool) function
can be used to distinguish value references from real references.

* [isBool](@ref doremir_from_bool)
* [isInt8](@ref doremir_from_bool)
* [isInt16](@ref doremir_from_bool)
* [isInt32](@ref doremir_from_bool)
* [isInt64](@ref doremir_from_bool)
* [isFloat](@ref doremir_from_bool)
* [isDouble](@ref doremir_from_bool)

## Extracting the value of a value reference {#ExtractingAValueReference}

The following functions extract the value of a value reference and destroy the associated
storage, if any. These functions can be applied directly, or by using 
@ref doremir_deep_destroy.

* [toBool](@ref doremir_from_bool)
* [toInt8](@ref doremir_from_bool)
* [toInt16](@ref doremir_from_bool)
* [toInt32](@ref doremir_from_bool)
* [toInt64](@ref doremir_from_bool)
* [toFloat](@ref doremir_from_bool)
* [toDouble](@ref doremir_from_bool)

In some cases it is useful to inspect the value of a reference without destroying it.
The *peek* functions can be used for that purpose. There are no peek functions for
small types, as they have no associated storage, and the *to* function can be used instead.

* [peekInt32](@ref doremir_from_bool)
* [peekInt64](@ref doremir_from_bool)
* [peekFloat](@ref doremir_from_bool)
* [peekDouble](@ref doremir_from_bool)


[persistent]: http://en.wikipedia.org/wiki/Persistent_data_structure
[util]: @ref doremir/util/literals.h
