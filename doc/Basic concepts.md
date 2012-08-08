
Basic concepts
==============

This chapter provides some brief definitions of the concepts used in the Audio Engine.

## Devices

A *device* is a provider of values of some type *a* and consumer of values of some type *b*. For
example, an audio device handles audio samples while a midi device handles midi messages. A
*source* is a device with a trivial output, while a *sink* is a device with a trivial input. A
device may or may not correspond to a physical device, such as a sound card or midi keyboard.

Devices are classified according to the type of input they provide, as well as their blocking
behavior.

## Sessions

A *session* provides a snapshot of devices currently available on the system. It prevents
accidental access to unavailable devices.

## Processors

A *processor* is the dual of a device: it provides a computation transforming input values of
some type *a* to output values of some type *b*. The Audio Engine provide several primitive
processors as well as wrappers for external audio plugins. Processors can be composed in a
combinatory fashion to describe processing networks.

Processors are classified according to the kind of computation they provide as well as other
properties such as sampling ratio, statefulness and atomicity.


## Signals

*Signal* simplifies the task of creating and connecting processors. A signal is a
time-varying value, which may be used with ordinary operators and functions. Functions from
signals to signals are equivalent to processors. For example, the following function describes
an attenuator.
  
    (define attenuate (x)
      (declare signal x)
      (* 0.2 x))


## Messages

A *message* is an value of some immutable type, used for communication between different parts
of an audio processing application. In contrast to signals, messages are non-blocking and
push-only: messages are sent by calling methods and received by registering callbacks. Messages
provide a way to interact with a running stream computation.

## Schedulers

FIXME Scheduler

## Dispatchers

FIXME Dispatcher

## Streams

A *stream* is a wrapper for one or more streaming computations, each involving a device
and a processor.

\pagebreak

