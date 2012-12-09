
Types
==========

We use the following notation for types:

Name                     | Description
------------------------ | ----------------------------
`int32, float32, string` | concrete types
`A`                      | type variable
`(A, B)`                 | pair of *A* and *B*
`[A x n]`                | vector of A
`~A`                     | signal of *A*
`A ~> B`                 | processor from *A* to *B*

Midi types
----------

The Audio Engine supports both simple messages and system exclusive (sysex) messages, represented by the
`midi_simple` and `midi_sysex` types respectively. The `midi_message` type is a discriminated union of
simple and system exclusive messages.

Midi processors typically operate in the list domain, i.e. `[a] ~> [b]`.


Audio types
----------

The Audio Engine performs all audio computation on floating-point values. Both 32-bit and 64-bit floats
are supported, represented by the `sample32` and `sample64` types respectively.

Audio processors typically operate in the array domain, i.e. `arrayN a ~> arrayN b`, for some *N > 0*.



\pagebreak

