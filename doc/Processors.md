

Audio processors
==========

The Audio Engine provides a high-level interface to signal processing, based on the signals and processors. A signal is a time-varying value derived from some external source, or generated internally in the audio engine. Similarly, a processor is a transformer function from signals to signals. In theory, the signal and processor concept could be applied to any data type, but the audio engine typically use 32-bit floating point values representing audio amplitude.

An audio processing network is created by applying processors to signals. Each application of a processor to a signal yields a new signal, similar to the application of functions to values in a programming language.


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

Ordinary C/C++ functions can be lifted to processors. 


Derived processors
----------


### Running processors in sequence

The `sequence(processors...)` function takes any number of processors and runs them in sequence.

### Running processors in parallel

The `parallel(processors...)` function takes any number of processors and runs them in parallel.

### Creating feedback loops

The `loop(processors)` function takes a single processor and splits it output into a loop, which is fed back into the original processor.


Other processors
----------

### Plugins

The Audio Engine provides a generic wrapping functionality for external audio-plugins such as AU or VST. Each instance of a plugin is cast as a distinct processor. Each plugin architecture provides its own functionality for loading and initalizing a plugin: for example VST plugins are loaded from binary files while AU plugins are loaded using the CoreServices component manager.

### Fluidsynth

The Audio Engine provides a wrapper for the Fluidsynth synthesizer. Fluidsynth generates audio using a collection of sampels known as a SoundFont.