
# Signals and processors {#Signals}

@anchor Signals
@anchor Processors
@tableofcontents

@note
    This page is under construction.

The most important concepts in the Audio Engine are the notion of *signals* and
*processors*. Both have simple definitions:

* A <em>[signal](@ref doremir_signal_t)</em> is a function of time, for example 
  \f$ y(t)=sin(2\pi\,t) \f$.

* A <em>[processor](@ref doremir_processor_t)</em> a function from a signal to a
  signal, for example \f$ y(t)=\frac{1}{2}\,x(t) \f$.

Both signals and processors can be built by *composition* of simple values: signals
can be composed to create more complex signals, and processors can be composed to
create more complex processors. Signals and processors interact by abstraction and
application: a function from signals to signals can be converted to a processor,
and a processor can be applied to a signal to yield another signal.

While many signals can be described by simple formulas, other signals such as
real-world audio recordings have no simple representation, and must be sampled to
be handled by a computer. The Audio Engine hide this complexity by representing
signals as opaque types. The problem of sampling and resampling are handled by
[input and output devices](@ref Devices). Thus signals are conceptually *continous*
and have neither sample rate or duration.


# Audio types {#SignalTypes}

Each signal has an associated *audio type*, describing the range of the
time function. Audio types are grouped into *simple*, *compound* and *special*.
For the runtime representation of audio types, see [this module](@ref DoremirType).

## Simple types {#Simple}

The *simple* audio types represent the amplitude range of the signal. They can be
grouped into *fixed-point* and *floating-point* types. As expected, the Audio
Engine processes fixed point data using modulo arithmetic, and floating-point data
types using floating-point arithmetic.

### Fixed-point types {#Int}

Type  | Description
------|------------------------------------------------------------------------
`i8`  | A 8-bit fixed-point number, range from \f$0\f$ to \f$2^{8}  - 1\f$
`i16` | A 16-bit fixed-point number, range from \f$0\f$ to \f$2^{16} - 1\f$
`i32` | A 32-bit fixed-point number, range from \f$0\f$ to \f$2^{32} - 1\f$
`i64` | A 64-bit fixed-point number, range from \f$0\f$ to \f$2^{64} - 1\f$

### Floating-point types {#Float}

Type  | Description
------|------------------------------------------------------------------------
`f32` | A 32-bit floating-point number, usually ranging from \f$-1\f$ to \f$1\f$
`f64` | A 64-bit floating-point number, usually ranging from \f$-1\f$ to \f$1\f$


## Compound types {#Compound}

The *compound* audio types are created by the combination of (simple or compound) audio 
types, written as follows.

Type            | Description
----------------|-----------------------------
`()`            | Unit type
`(a, a)`        | Pair of \f$a\f$ and \f$b\f$
`[a x N]`       | Vector of \f$a\f$
`{a}`           | Frame of \f$a\f$


### Pairs {#Pairs}

Pairs represent multichannel audio of possibly different types. For example, a
signal of 2 channels of 32-bit floating point audio would have the type `(f32,f32)`.

More complex channel configurations can be constructed by nested pairs. For example
a a three-channel stream could be represented as `(a,(b,c))`. Note that by
convention, nested pairs associate to the right.

### Vectors {#Vectors}

Vectors represent multichannel audio of a single type. The vector type `[a x 1]` is
equivalent to `a`, and `[a x N]` is equivalent to `(a,[a x N-1])`. For example, a signal
of 10 channels of 32-bit floating point audio would have the type `[f32 x 10]`.

Vectors can also be used to represent resampled audio. For example, an upsampling
processor might take a signal of type `{f32}` as input, and return a signal of type
`[{f32} x 2]` as output, meaning that the output have twice the amount of samples
as the input. A downsampling processor might in turn take an input of type `[{f32}
x 2]` and output a signal of type `{f32}`. This makes it possible to describe
the density of information in and audio signal without having to describe how it
is represented. 

Vectors and pairs can be used together to represent different kinds of interleaved
samples. For example `[(f32,f32) x 1024]` means a sequence of 1024 pairs of
samples, while `([f32 x 1024],[f32 x 1024])` means a pair of sequences of 1024
samples.

### Frames {#Frames}

Frames are special vectors of an unspecified length. The actual amount of samples
in a frame is determined at runtime and may vary during a single audio session. The
*buffer size* of an audio stream is the maximum length of a frame, and is typically
a multiple of two.

Frames make it possible to embed audio and control rate streams in a single signal.
For example the type of a low-frequency oscillator could be `f32 ~> {f32}` where
the input type changes every frame instead of every sample.


## Special types {#Special}

The *special* types are the types of signals and processors, written as follows.

Type            | Description
----------------|-----------------------------
`~a`            | signal of \f$a\f$
`a ~> b`        | processor from \f$a\f$ to \f$b\f$




# Using signals {#id10569}

TODO

## Creating signals {#id16916}

### Signals from numbers {#id19230}

Numbers can be converted to signals using @ref doremir_signal_constant. The
resulting signal is a constant function that ignores the incoming time.

\f$
    y(t) = c
\f$

### Signals from events {#id11324}

Events can be converted to signals using @ref doremir_signal_value. Each occurence
will update the given signal to the incoming value.

### Signals from buffers {#id13431}

TODO

### Signals from devices {#id1298921}

TODO

## Modifying signals {#id29154}

### Unary operators {#id6220}

TODO

### Binary operators {#id13489}

TODO

### Applying processors to signals {#id4828}

TODO

## Delayed signals {#id15466}

### The time signal {#id29844}

TODO

### Simple delays {#id29841}

TODO

### Recursive delay {#id4345}

TODO

## Working with routing {#id19230}

TODO




# Using processors {#id6587}

## Creating processors {#id3674123}

### Identity processor {#id2712183}

### Constant processor {#id223183}

### Split processor {#id314133}

@image html  dsp_split.png "A processor"
@image latex dsp_split.pdf "A processor" width=0.6\textwidth

### Unary processors {#id61221230}

TODO
@image html  dsp_unary.png "A processor"
@image latex dsp_unary.pdf "A processor" width=0.6\textwidth

### Binary processors {#id14512489}

@image html  dsp_binary.png "A processor"
@image latex dsp_binary.pdf "A processor" width=0.6\textwidth



## Modifying processors {#id19739}

### Sequential composition {#id4287}

Sequential processors.

@image html  dsp_seq.png "A processor"
@image latex dsp_seq.pdf "A processor" width=0.8\textwidth

### Parallel composition {#id12464}

Sequential processors.

@image html  dsp_par.png "A processor"
@image latex dsp_par.pdf "A processor" width=0.6\textwidth

### Recursive composition {#id19712333}

Recursive processors.

@image html  dsp_loop.png "A processor"
@image latex dsp_loop.pdf "A processor" width=0.6\textwidth


## External processors {#id25821265}


### FluidSynth {#id17205}

TODO

### Audio Units {#id8544}

TODO

### VST {#id792}

TODO






