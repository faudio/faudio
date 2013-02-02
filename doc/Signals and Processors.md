
# Signals and processors {#Signals}

@anchor Signals
@anchor Processors
@tableofcontents

@note
    This page is under construction.

The core abstrations in the Audio Engine are *signals* and *processors*, both of
which have simple and precise definitions:

* A *signal* is a function of time. An example is the sinusoid \f$ y(t)=sin(2\pi\,440) \f$.

* A *processor* a function from a signal to a signal. An example is the attenuator
\f$ y(t)=x(t)*0.5 \f$.

Both signals and processors can be built by *composition* of simple values: signals
can be composed to create multichannel signals, and processors can be composed to
create more complex processing algorithms. Signals and processors interact by
abstraction and application: a function from signals to signals can be *lifted*
to a processor, and the application of a processor to a signal yields a signal.

While many signals can be described by simple formulas such as the preceding, some
signals are extremely complex and requires simplification to be represented in a
computer system. The Audio Engine hides this complexity from you by representing
all signals and processors by opaque types. Because signals are continuous, there
is no notion of a sample rate in a signal value, these are handled by
[devices](@ref Devices).


# Audio types {#SignalTypes}

Each signal has an associated *audio type*, describing the range of the
time function. Audio types are grouped into *simple*, *compound* and *special*.
For the runtime representation of audio types, see [this module](@ref DoremirType).

## Simple types {#Simple}

The *simple* audio types represent the amplitude range of the signal. They can be
grouped into *fixed-point* and *floating-point* types. As expected, the Audio
Engine processes fixed point data use modulo arithmetic, and floating-point data
types use floating-point arithmetic.

Note that while many most modern signal processing applications usually rely mostly
on floating-point arithmetic, there are some algorithms and processors that benefit
from using fixed-point arithmetic. The Audio Engine allow the client to mix the
types of audio signals freely in a single application.


### Fixed-point types {#Int}

Type  | Description
------|------------------------------------------------------------------------
`i8`  | A 8-bit fixed-point sample, range from \f$0\f$ to \f$2^{8}  - 1\f$
`i16` | A 16-bit fixed-point sample, range from \f$0\f$ to \f$2^{16} - 1\f$
`i32` | A 32-bit fixed-point sample, range from \f$0\f$ to \f$2^{32} - 1\f$
`i64` | A 64-bit fixed-point sample, range from \f$0\f$ to \f$2^{64} - 1\f$

### Floating-point types {#Float}

Type  | Description
------|------------------------------------------------------------------------
`f32` | A 32-bit floating-point sample, usually ranging from \f$-1\f$ to \f$1\f$
`f64` | A 64-bit floating-point sample, usually ranging from \f$-1\f$ to \f$1\f$


## Compound types {#Compound}

The *compound* audio types are created by the combination of (simple or compound) audio 
types, written as follows.

Type            | Description
----------------|-----------------------------
`(a, a)`        | pair of \f$a\f$ and \f$b\f$
`[a x N]`       | vector of \f$a\f$
`{a}`           | frame of \f$a\f$


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
x 2]` and output a signal of type `{f32}`.

Vectors and pairs can be used together to represent different kinds of interleaved
samples. For example `[(f32,f32) x 1024]` means a sequence of 1024 pairs of
samples, while `([f32 x 1024],[f32 x 1024])` means a pair of sequences of 1024
samples.

### Frames {#Frames}

Frames are special vectors of an unspecified length. The actual
amount of samples in a frame is determined at runtime and may vary during a single
audio session. The *buffer size* of an audio stream is the maximum length of a
frame, and is typically a multiple of two.


## Special types {#Special}

The *special* types are the types of signals and processors, written as follows.

Type            | Description
----------------|-----------------------------
`~a`            | signal of \f$a\f$
`a ~> b`        | processor from \f$a\f$ to \f$b\f$








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






