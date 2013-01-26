
# Data structures {#DataStructures}

@anchor DataStructures
@tableofcontents

@note
    This page is under construction.

The Audio Engine include a set of immutable data structures, which are primarily
used for message passing. Immutability allows data structures to be passed between
threads with little effort.


# Memory management {#MemoryManagement}

The Audio Engine use *single ownership* for all of its reference types. Each data
structure provides one or more *constructor functions*. Such functions create and
return a single value, thereby entrusting the caller to pass the same value to a
*destructive function* later on. There may also be a copy function, which is a special
form of constructor that takes an argument of the same type.

The [generic](@ref GenericFunctions) functions [copy](@ref doremir_buffer_copy) and
@ref [destroy](@ref doremir_buffer_poke) can also be used.

Functions operating on data structures come in two variants: a non-destructive
variant that simply construct a new data structure (the default), and a destructive
variant which destructs the old data structures while constructing the new one. The
destructive functions should be used whenever a variable is updated locally. Note
that the destructive functions *destroys* the original data structure rather
than *mutating* it.


# Value references {#ValueReferences}

Value types can be converted to reference types using a set of conversion
functions. These functions take a non reference type and return a so called value
reference. A value reference is a proper reference that can be safely stored in a
data structure, it can subsequently be destroyed and its value extracted.

Like other data structures, value references may be created and destroyed from any
thread and have single ownership semantics.

* `isBool`
* `isInt8`
* `isInt16`
* `isInt32`
* `isInt64`
* `isFloat`
* `isDouble`
* `isRef`

* `toBool`
* `toInt8`
* `toInt16`
* `toInt32`
* `toInt64`
* `toFloat`
* `toDouble`

* `destroyBool`
* `peekInt8`
* `peekInt16`
* `peekInt32`
* `peekInt64`
* `peekFloat`
* `peekDouble`

* `fromBool`
* `fromInt8`
* `fromInt16`
* `fromInt32`
* `fromInt64`
* `fromFloat`
* `fromDouble`
