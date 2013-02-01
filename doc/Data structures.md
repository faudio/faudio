
# Data structures {#DataStructures}

@anchor DataStructures
@tableofcontents

@note
    This page is under construction.

The Audio Engine include a set of persistent data structures, which are primarily
used for message passing.

All data structures can store all reference types, including [value references](@ref ValueReferences).

All data structures can be created and destroyed from any thread, as long as the
thread establish an order between creation and destruction.


# Overview {#Overview}

* [Pairs](@ref DoremirPair)
* [Lists](@ref DoremirList)
* [Sets](@ref DoremirSet)
* [Maps](@ref DoremirMap)
* [Graphs](@ref DoremirGraph)

# Conventions {#Conventions}

## Literals {#Literals}

- `pair(1, 2)`
- `list(1, 2, 3)`
- `set(1, 2, 3)`
- `map(string("foo"), i32(1), 
      string("bar"),  i32(2))`
- `graph(i32(1), i32(2), connect(i32(1), i32(2), i32(3)))`

## Show {#Literals}

- `(1,2)`
- `[1,2,3]`
- `{1,2,3}`
- `{foo:1, bar:2}`
- `{1, 2, 1->2: 3}`

## Creation, copying and destruction {#CreateCopyDestroy}

TODO

TODO destructive operations

The Audio Engine use *single ownership* for all of its reference types.

TODO

# Value references {#ValueReferences}

Value types can be converted to reference types using a set of conversion
functions. These functions take a non reference type and return a so called value
reference. A value reference is a proper reference that can be safely stored in a
data structure; it can subsequently be destroyed and its value extracted. Like
other data structures, value references may be created and destroyed from any
thread and have single ownership semantics.

It is not specified exactly how value references are implemented; however the reference
representation of a value typically have a different bit pattern from the represented
value. It is not safe to cast a value (such as an integer) to a pointer treat the resulting
address as a value reference; conversely, it is not safe to treat a value reference as a pointer
(i.e. dereferencing it, incrementing it etc). It is guaranteed that the value references will never
overlap with real references as returned by the system allocator.

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
