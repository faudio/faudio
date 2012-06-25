

Audio processors
==========

The Audio Engine provides a high-level interface to signal processing, based on the signals and processors. A signal is a time-varying value derived from some external source, or generated internally in the audio engine. Similarly, a processor is a transformer function from signals to signals. In theory, the signal and processor concept could be applied to any data type, but the audio engine typically use 32-bit floating point values representing audio amplitude.

An audio processing network is created by applying processors to signals. Each application of a processor to a signal yields a new signal, similar to the application of functions to values in a programming language.


Primitive processors
----------

### The constant processor
### The identity processor
### The split processor
### The delay processor


Derived processors
----------

### Running processors in sequence
### Running processors in parallel
### Using feedback


Other processors
----------

### Plugins

The Audio Engine provides a wrapping functionality for external audio-plugins such as AU or VST. Each instance of a plugin is cast as a distinct processor.

### Fluidsynth

Fluidsynth