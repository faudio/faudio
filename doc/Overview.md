
Basic concepts
--------------

This chapter provides some brief definitions of the concepts used in the Audio Engine.

### Future and improving values

The Audio Engine use an opaque asynchronous interface. Two special kinds of asynchronous values
are provided. A *future* value represents a value which may not be immediatly available. An
*improving* value represents a value which may gradually approach its final value.

### Sessions

A *session* provides a snapshot of devices currently available on the system.

### Devices

A *device* is a provider of values of some type *B* and consumer of values of some type *A*. For
example, an audio device provides and consumes audio samples while a midi device provides and
consumes midi messages. A device may or may not correspond to a physical device, such as a sound
card or midi keyboard.

A device with no output is called a *source*, while a device with no input is called a *sink*.

### Processors

A *processor* provides a computation transforming a stream of values of some type *A* to a
stream of values of some type *B*. The Audio Engine provide several primitive processors as well
as wrappers for external audio plugins. Processors can be *composed* to create new processors.

A processor with no input is often called an *unfold* or *synth*, while a processor with no
input is called a *fold* or an *analyzer*. An intermediate processor is called a *map* or a
*filter*.

### Signals

A signal is a stream of values of some type *A*. Signals and processors interact by application,
specifically a processor from *A* to *B* may be applied to a signal of *A* to yield a signal of
*B*.

### Messages

A *message* is an value of some type *M*, used for communication between different parts
of an audio processing application. *Senders* and *receiver* emits and reacts to messages
respectively. *Dispatchers* provides routing points where messages can be sent and received.

### Schedulers

A *scheduler* provides timely execution of *tasks*.

### Streams

A *stream* is a wrapper for one or more streaming computations, each involving a device and a
processor. Processors may interact with each other and the outside world by messages. Each
stream provides a scheduler and a dispatcher to manage communication.

![Overview of the concepts used in the Audio Engine](images/stream.eps "")


\pagebreak

