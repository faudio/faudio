
# Data structures {#DataStructures}

@anchor DataStructures

@tableofcontents

The Audio Engine include a set of immutable data structures, which are primarily used
for message passing. Immutability allows data structures to be passed between threads.
The data structures have single-owership semantics and store reference types.


# Single-ownership {#si}

The Audio Engine use single-ownership semantics for all of its reference types.
Each data structure provides one copy and one destruct function. Each data
structure created by a create or copy function must be destroyed by passing it to a
destroy function. The [generic](@ref GenericFunctions) functions @ref doremir_copy
and @ref doremir_destroy can also be used.

Functions operating on data structures come in two variants: a non-destructive
variant that simply construct a new data structure (the default), and a destructive
variant which destructs the old data structures while constructing the new one. The
destructive functions should be used whenever a variable is updated locally. Note
that the destructive functions # destroys the original data structure rather
than *mutating* it.


# Value references

Value types can be converted to reference types using a set of conversion
functions. These functions take a non reference type and return a pointer-sized
value that can be safely stored in a data structure. Like other data structures,
value references may be created and destroyed from any thread and have
single-ownership semantics.

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

* `peekBool`
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
