
# Data structures {#DataStructures}

@anchor DataStructures
@tableofcontents

@note
    This page is under construction.

Fa include a set of general purpose [persistent&nbsp;data&nbsp;structures][persistent],
which are primarily used for message passing between the audio thread and other threads.
The fact that the data structures are persistent eliminate many of the problems commonly
associated with multi-threaded programming.

The data structures in Fa are somewhat different from the structures
found in most languages, in that they have single-ownership semantics. This
eliminates the need for a garbage collector while still allowing a high-level
programming style. To understand single-ownership semantics, you should read the
section about [creation&nbsp;and&nbsp;destruction](@ref CreateCopyDestroy) below.
All data structures are polymorphic over reference types, see the section on
[type&nbsp;safety](@ref id19466) for more details. 

Note that there is no interface capturing the notion of a data structure: they are
simply reference types obeying the conventions described below. However, all data
structures support generic [equality](@ref fa_equal_t) or 
[ordering](@ref fa_order_t), [copying](@ref fa_copy_t) and 
[destruction](@ref fa_destroy_t).


# Overview {#Overview}

The core data structures are:

Type                           | Semantics
-------------------------------|------------------------------------------------------
[Pair](@ref FaPair)       | An ordered pair
[List](@ref FaList)       | An ordered sequence
[Set](@ref FaSet)         | An ordered set
[Map](@ref FaMap)         | A set of ordered pairs
[String](@ref FaString)   | A sequence of Unicode characters
<!-- [Graph](@ref FaGraph)     | A labeled, directed graph -->

There is also a set of *mutable* data structures not included in this table. These
are used internally in Fa and need rarely be accessed by the user. For
completeness, they are:

Type                                              | Semantics
--------------------------------------------------|------------------------------------------------------
[Atomic](@ref FaAtomic)                      | An atomic reference
[Atomic queue](@ref FaAtomicQueue)           | A first-in, first-out atomic queue
[Atomic stack](@ref FaAtomicStack)           | A last-in, first-out atomic queue
[Buffer](@ref FaBuffer)                      | A byte-level mutable buffer
<!--
[Atomic ringbuffer](@ref FaAtomicRingBuffer) | A byte-level, bounded, first-in, first-out atomic queue
-->
<!--
[Priority queue](@ref FaPriorityQueue)       | A first-in, ordered out priority queue
-->



# Using data structures {#Conventions}

## Creation and destruction {#CreateCopyDestroy}

TODO

## Creating from strings {#Literals}

### Show

When [printed](@ref fa_print), the data structures are rendered in a language-neutral form. 

- `(1,2)`
- `[1,2,3]`
- `{1,2,3}`
- `{foo:1,bar:2}`
- `({1,2,3},{((1,2),"foo"),((1,3):"bar")})`

### Literals

All data structures have *literals* defined in the [utility&nbsp;headers][util]. A
literal expression always evaluate to a newly created instance of the data
structure. Beware not to pass a number to the literals, you must use
[value&nbsp;references](@ref ValueReferences).

- `pair(i32(1),i32(2))`
- `list(i32(1),i32(2),i32(3))`
- `set(i32(1),i32(2),i32(3))`
- `map(string("foo"),i32(1),string("bar"),i32(2))`
- `graph(i32(1),i32(2),edge(i32(1),i32(2),i32(3)))`

### JSON

TODO

## Accessing all elements {#ForEach}

TODO

## Mapping, folding and filtering {#MapFoldFilter}

TODO

## Thread safety {#ThreadSafety}

All operations on data structures are thread-safe except creation, copying and
destruction, which are subject to the following restrictions:

* If a data structure *a* is created in thread *t* and used in thread *u*,
  an ordering must be established between the creation function returning
  in *t* and subsequent use of *a* in *u*. 
  
* If a data structure *a* is used in thread *t* and destroyed in thread *u*,
  an ordering must be established between the last use of *a* in *t* and 
  the destructive function being applied in *u*. 

To establish an ordering, you should either synchronize the threads *t* and *u*, or
transfer the data structure in an atomic variable or queue. Note that copying is
considered both *usage* of the copied value and a *creation* of the copy. Also some
functions (such as @ref fa_list_dcons) are both destructive on its input, and
constructive on its output.

## Type safety {#id19466}

All data structures can store all reference types, including
[value&nbsp;references](@ref ValueReferences). The user must rely on guarantees
outside the scope of the compiler to assure that the extracted elements are of the
right type. There are several ways to do this:

* Assure that the structure contains a specific type.
    * For example, many functions in Fa API return pairs and lists, their
      documentation clearly stating what type of elements the list will contain.
* Assure that the structure contains a generic type.
    * For example, a function may require a set of values implementing [Show](@ref fa_string_show_t).

In some cases, it does not matter what type a data structure contains, as the
elements are not going to be inspected. For example, @ref fa_list_reverse can
receive a list of any type, as it operates purely on the structure of the list and
does not need to use its values. 



# Value references {#id182783728273}
@anchor ValueReferences

Value types can be converted to reference types using a set of conversion
functions. These functions take a non reference type and return a so called *value
reference*. This reference can subsequently be destroyed and its value extracted.

A value reference is a proper reference that can be safely stored in a data
structure. Like other data structures, value references may be created and
destroyed from any thread and have single ownership semantics. Value references are
in fact tiny data structures containing a single element. They support all normal
data structure operations including [equality](@ref fa_equal_t) or 
[ordering](@ref fa_order_t), [copying](@ref fa_copy_t) and 
[destruction](@ref fa_destroy_t). 
In addition, they also support [arithmetic](@ref fa_number_t).

It is not specified exactly how value references are implemented; however the
reference representation of a value typically have a different bit pattern from the
represented value. It is not safe to cast a value of a primitive type, such as an
integer, to a pointer and treat the resulting address as a value reference.
Conversely, it is not safe to treat a value reference as a pointer by dereferencing
it, or doing pointer arithmetic. It is guaranteed that the value references will
never overlap with real references. 

<!--
## Creating a value reference {#id817283782}

Value references are created by the following functions:

* [fa_from_bool](@ref fa_from_bool) or [b](@ref fa_from_bool)
* [fa_from_int8](@ref fa_from_int8) or [i8](@ref fa_from_int8)
* [fa_from_int16](@ref fa_from_int16) or [i16](@ref fa_from_int16)
* [fa_from_int32](@ref fa_from_int32) or [i32](@ref fa_from_int32)
* [fa_from_int64](@ref fa_from_int64) or [i64](@ref fa_from_int64)
* [fa_from_float](@ref fa_from_float) or [f32](@ref fa_from_float)
* [fa_from_double](@ref fa_from_double) or [f64](@ref fa_from_double)

## Checking the type of a value reference {#CheckingTypeValueReference}

The [fa_is_ref](@ref fa_is_ref) and [fa_is_value](@ref fa_is_value) function
can be used to distinguish value references from real references.

* [fa_is_bool](@ref fa_is_bool) or [qb](@ref fa_is_bool)
* [fa_is_int8](@ref fa_is_int8) or [qi8](@ref fa_is_int8)
* [fa_is_int16](@ref fa_is_int16) or [qi16](@ref fa_is_int16)
* [fa_is_int32](@ref fa_is_int32) or [qi32](@ref fa_is_int32)
* [fa_is_int64](@ref fa_is_int64) or [qi64](@ref fa_is_int64)
* [fa_is_float](@ref fa_is_float) or [qf32](@ref fa_is_float)
* [fa_is_double](@ref fa_is_double) or [qf64](@ref fa_is_double)

## Extracting the value of a value reference {#ExtractingAValueReference}

The following functions extract the value of a value reference and destroy the associated
storage, if any. These functions can be applied directly, or by using 
@ref fa_deep_destroy.

* [fa_to_bool](@ref fa_to_bool) or [tb](@ref fa_to_bool)
* [fa_to_int8](@ref fa_to_int8) or [ti8](@ref fa_to_int8)
* [fa_to_int16](@ref fa_to_int16) or [ti16](@ref fa_to_int16)
* [fa_to_int32](@ref fa_to_int32) or [ti32](@ref fa_to_int32)
* [fa_to_int64](@ref fa_to_int64) or [ti64](@ref fa_to_int64)
* [fa_to_float](@ref fa_to_float) or [tf32](@ref fa_to_float)
* [fa_to_double](@ref fa_to_double) or [tf64](@ref fa_to_double)

In some cases it is useful to inspect the value of a reference without destroying it.
The *peek* functions can be used for that purpose. There are no peek functions for
small types, as they have no associated storage, and the *to* function can be used instead.

* [fa_peek_int32](@ref fa_peek_int32) or [pi32](@ref fa_peek_int32)
* [fa_peek_int64](@ref fa_peek_int64) or [pi64](@ref fa_peek_int64)
* [fa_peek_float](@ref fa_peek_float) or [pf32](@ref fa_peek_float)
* [fa_peek_double](@ref fa_peek_double) or [pf64](@ref fa_peek_double) -->



[persistent]: http://en.wikipedia.org/wiki/Persistent_data_structure
[util]: @ref fa/util/literals.h
