

Some notes about the C API
========

Generally, a C++ API can only be consumed by other C++ code.

This directory contains a wrapper C API, which can be consumed by any language
with a foreign interface to C. It can also be used dynamically from libffi-like
libraries. It is designed to be usable from all kinds of languages, so some
kind of common type system is required. This type system is described below.

We will use the term client language to mean any language consuming the C API.

## Simple types
 
Simple types include integers, enumerations and floating point values. Values
of these types are always passed by value on the stack and can be manipulated
by the client language in any way.

The supported simple types are:

    int
    int16_t
    int32_t
    int64_t
    unsigned int
    uint16_t
    uint32_t
    uint64_t
    char
    signed char
    unsigned char
    float
    double

Enumeration types are represented by `int` in the C API.


## Reference types

The reference types are modeled by the `Reference` concept, and include
some kind of memory management. They are passed as opaque pointers. 

Higher-level languages will generally wrap these in some container type that
dispatches to the underlying reference.

Any method returning a reference type will specify some ownership semantic,
typically the client language creates such objects by calling a `create` method
and releases them by calling a `free` method. Languages with built-in memory
management may use a garbage-collector hook to invoke the free method.


## Function types

Pointers to C-style functions can be passed to and returned from the C API
as usual.

## Structured types

Structured types are polymorphic container types such as lists and pairs.
Although such types generally have value semantics, they must be passed by
reference as the C language does not support parametric polymorphism.

Higher-level client languages should provide some glue code to ensure the
value-like semantics. Generally this amounts to allocate an object using a
`create` method, copying the value into the dynamic object, pass it to the
API function (where it will be copied), and then release it by calling a
`free` method.

Anything convertible to `scl::sequence` can be passed as a lazy sequence.
In this case the host language may forego copying the native object, instead
wrapping it in a container type that dispatches to the underlying container
(similar to an ordinary reference type).

Functions accepting or returning sequences may provide overloaded versions
for C arrays as well as sequences. These are generally suffixed by `array`.


## Dynamic types

Dynamic types are supported by the `scl::dynamic::type` class. This type
can contain all types modeling `Dynamic`, as well as pairs and sequences 
thereof. The types modeling dynamic include all simple types, as well
as the reference types exposed in the C API.

## Error handling

FIXME





