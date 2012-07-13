

Audio processors
==========

The audio engine provides a high-level interface to DSP, based on the *signals* and *processors*. 

A signal is a time-varying value, which may be derived from some external source, or generated internally in the audio engine. Examples of external sources are microphones, sound files on the hard drive or chunks of audio received over a network socket. Examples of signals generated internally are accumulating signals (which change over time according to some formula) or constant signals (which never change).

A processor is a transformer function from signals to signals. Examples are low-pass filters, delay lines or reverb units. 

A *processing network* is created by applying processors to signals. Each application yields a new signal which may be applied by further processors. A processing graph can be defined explicitly, by applying signals, or implicitly by composing processors. Internally, the audio engine optimizes signals and processors so that the style used to program the network has no direct effect on the output.


Primitive processors
----------

### The constant processor

The constant processor takes an input signal which it ignores, and outputs a constant value.

### The identity processor

The identity processor takes an input signal which it simply forwards to its output.

### The split processor

The split processor takes an input signal and splits into *n* equal signals, where *n >= 2*. It outputs *n* signals.

### The delay processor

The delay processor takes an input signal and outputs the same signal delayed by *n* samples.

### Lifted processors

Ordinary functions can be lifted to processors.

Lifted processors are provided for all functions defined in the header `<math.h>` as well as for all standard operators on fixed and floating-point values.


Derived processors
----------


### Running processors in sequence

The `sequence(processors...)` function takes any number of processors and runs them in sequence.

### Running processors in parallel

The `parallel(processors...)` function takes any number of processors and runs them in parallel.

### Creating feedback loops

The `loop(processors)` function takes a single processor and splits it output into a loop, which is fed back into the original processor.



Stateful vs non-stateful processors
----------

Stateful processors accept messages to change their state.

FIXME conversions

    stage   :: (m ! a ~> b)    -> (([m], a) ~> b)
    unstage :: (([m], a) ~> b) -> (m ! a ~> b)





External processors
----------

### Plugins

The Audio Engine provides a generic wrapping functionality for external audio-plugins such as AU or VST. Each instance of a plugin is cast as a distinct processor. Each plugin architecture provides its own functionality for loading and initalizing a plugin: for example VST plugins are loaded from binary files while AU plugins are loaded using the native component manager.

### Fluidsynth

The Audio Engine provides a wrapper for the Fluidsynth synthesizer. Fluidsynth generates audio using a collection of sampels known as a SoundFont.

### Custom processors

New processors may be added by implemnting the `AudioProcessor` interface.




