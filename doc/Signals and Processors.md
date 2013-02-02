
# Signals and processors {#Signals}

@anchor Signals
@anchor Processors
@tableofcontents

@note
    This page is under construction.

TODO signals and processors

# Audio types {#SignalTypes}

TODO

## Simple types {#Simple}

The *simple* types represent a single audio sample. They can be grouped into
*integer* and *floating-point* types.

### Integer types {#Int}

Type  | Description
------|------------------------------------------------------------------------
`i8`  | A 8-bit integer sample, range from \f$0\f$ to \f$2^{8}  - 1\f$
`i16` | A 16-bit integer sample, range from \f$0\f$ to \f$2^{16} - 1\f$
`i32` | A 32-bit integer sample, range from \f$0\f$ to \f$2^{32} - 1\f$
`i64` | A 64-bit integer sample, range from \f$0\f$ to \f$2^{64} - 1\f$

### Floating-point types {#Float}

Type  | Description
------|------------------------------------------------------------------------
`f32` | A 32-bit floating point sample, usually ranging from \f$-1\f$ to \f$1\f$
`f64` | A 64-bit floating point sample, usually ranging from \f$-1\f$ to \f$1\f$

Note that while most signal processing libraries use 32-bit or 64-bit floating
point there are some algorithms and processors that benefit from using integer
arithmetic rather than floating point. The type system allow the client to mix the
types of audio signals freely in a single application.


## Compound types {#Compound}

### Pairs {#Pairs}

Pairs, written as `(a,b)`, represent multichannel audio. For example, given a
single audio channel of type `f32`, a stereo version of the signal would have the
type `(f32,f32)`. The components of a pair need not be the same, and can be any
type.

More complex channel configurations can be constructed by nested pairs. For example
a a three-channel stream could be represented as `(a,(b,c))`. Note that by
convention, nested pairs associate to the right. 

<!--
The multichannel functions can be
used to quickly construct a multichannel type.
-->

### Vectors {#Vectors}

Vectors, written as `[a x N]` where `N` is a whole number, represent resampled
audio. For example, an upsampling processor might take a signal of type `{f32}` as
input, and return a signal of type `[{f32} x 2]` as output, meaning that the output
have twice the amount of samples as the input. A downsampling processor might in
turn take an input of type `[{f32} x 2]` and output a signal of type `{f32}`.

<!--
A signal of type `[a x 1]` is equivalent to `a`, a signal of type `[a x 2]` to
`(a,a)`, of `[a x 3]` to `(a,(a,a))` and so on. Because of this correspondence of
vectors and pairs, vectors can be used to encode multichannel audio of a single
type, i.e. instead of `(f32,(f32,f32))` one can use `[f32 x 3]`.
-->

Vectors and pairs can be used together to represent different kinds of interleaved
samples. For example `[(f32,f32) x 1024]` means a sequence of 1024 pairs of
samples, while `([f32 x 1024],[f32 x 1024])` means a pair of sequences of 1024
samples.


### Frames {#Frames}

Frames, written as `{a}` are special vectors of an unspecified length. The actual
amount of samples in a frame is determined at runtime and may vary during a single
audio session. The *buffer size* of an audio stream is the maximum length of a
vector, and is typically a multiple of two.


## Special types {#Special}

### Signal type {#SigType}
### Processor type {#ProcType}

# Using signals {#Comb}

## Application {#SApp}
## Lifting to signal level {#Lift}
## Signals as buffers {#SigBuf}
## Buffers as signals {#BufSig}
## Built-in signals {#BuiltInSig}


# Using processors {#Proc}

## Routing {#Routing}

### Sequential {#seq}

Sequential processors.

@image html  dsp_seq.png "A processor"
@image latex dsp_seq.pdf "A processor" width=0.8\textwidth

### Parallel {#par}

Sequential processors.

@image html  dsp_par.png "A processor"
@image latex dsp_par.pdf "A processor" width=0.6\textwidth

### Recursive {#loop}

Recursive processors.

@image html  dsp_loop.png "A processor"
@image latex dsp_loop.pdf "A processor" width=0.6\textwidth

### Time processors {#dekay}


### Folds and unfolds {#fold}

Folding and unfolding processors processors.

@image html  dsp_split.png "A processor"
@image latex dsp_split.pdf "A processor" width=0.6\textwidth
@image html  dsp_binary.png "A processor"
@image latex dsp_binary.pdf "A processor" width=0.6\textwidth

### Map {#lift}

Mapping processors.

@image html  dsp_unary.png "A processor"
@image latex dsp_unary.pdf "A processor" width=0.6\textwidth

## Built-in processors {#BuiltInProc}

### Time {#Id}
### Constant {#Const}
### Delay {#Delay}


## Buffering and recording {#BufRec}

### Record and play {#RecPlay}

### Record and play to the file system {#FileRecPlay}

### Routing to non-real time devices {#NRTRoute}


## External processors {#ExternalProcessors}

### FluidSynth {#FluidSynth}

TODO

### Audio Units {#AudioUnits}

TODO

### VST {#VST}

TODO






