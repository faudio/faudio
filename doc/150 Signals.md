
# Signals {#Signals}

@anchor Signals
@tableofcontents

A *signal* is a time-varying value, or more formally, a function of time. Typically the value is a number
representing audio amplitude, but this need not be the case: signals may represent any time-varying value,
including audio, video, image or other forms of data. Signals can be transformed by 
[processors](@ref Processors), which are functions from signals to signals.

# Types {#SignalTypes}

The Audio Engine provides a [type system](@ref DoremirType) for signals which are checked whenever a signal is
applied to a processor, or a higher-order signal or processor function is called. This prevents the client from
creating erronous signal networks, such as connections from a two-channel output to a one-channel input.


## Simple types {#Simple}

The *simple* types represent a single audio sample. They can be grouped into *integer* and *floating-point*
types.

### Integer types {#Int}

- `i8`
  * an 8-bit integer sample, ranging from `0` to `255`
- `i16`
  * A 16-bit integer sample, ranging from `0` to `65535`
- `i32`
  * A 32-bit integer sample, ranging from `0` to `4294967295`
- `i64`
  * A 64-bit integer sample, ranging from `0` to `18446744073709551615`

### Floating-point types {#Float}

- `f32`
  * A 32-bit floating point sample, usually ranging from `-1.0` to `1.0`
- `f64`
  * A 64-bit floating point sample, usually ranging from `-1.0` to `1.0`

Note that while most modern DSP libraries use 32-bit floating point (i.e. `f32`), there are some algorithms and
processors that benefit from using integer arithmetic rather than floating point. The type system allow the
client to mix the types of audio signals freely in a single application.

## Compound types {#Compound}

### Pairs {#Pairs}

Pairs, written as `(a,b)`, represent multichannel audio. For example, given a single audio channel of type
`f32`, a stereo version of the signal would have the type `(f32,f32)`. The components of a pair need not be the
same, and can be any type.

More complex channel configurations can be constructed by nested pairs. For example a a four-channel stream
could be represented as `(a,(b,(c,d)))`. By convention, nested pairs associate to the right, that is as
`(a,(b,c))`, rather than `((a,b),c)`. The multichannel functions can be used to quickly construct a multichannel
type.

### Vectors {#Vectors}

Vectors, written as `[a x N]` where `N` is a whole number, represent resampled audio. For example, an upsampling
processor might take a signal of type `{f32}` as input, and return a signal of type `[{f32} x 2]` as output,
meaning that the output have twice the amount of samples as the input. A downsampling processor might in turn
take an input of type `[{f32} x 2]` and output a signal of type `{f32}`.

A signal of type `[a x 1]` is equivalent to `a`, a signal of type `[a x 2]` to `(a,a)`, of `[a x 3]` to
`(a,(a,a))` and so on. Because of this correspondence of vectors and pairs, vectors can be used to encode
multichannel audio of a single type, i.e. instead of `(f32,(f32,f32))` one can use `[f32 x 3]`.

Vectors and pairs can be used together to represent different kinds of interleaved samples. For example
`[(f32,f32) x 1024]` means a sequence of 1024 pairs of samples, while `([f32 x 1024],[f32 x 1024])` means a pair
of sequences of 1024 samples.


### Frames {#Frames}

Frames, written as `{a}` are special vectors of an unspecified length. The actual amount of samples in a frame
is determined at runtime and may vary during a single audio session. The *buffer size* of an audio stream is the
maximum length of a vector, and is typically a multiple of two, such as 256, 512 or 1024.

# Continous vs. discrete {#Cont}

While real-world signals are continous in nature, digital signal processing invariably involves some form of 
sampling 

# Application of processors to signals {#Procs}

# Processors as functions of signals {#Procs}

