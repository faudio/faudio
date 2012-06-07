Reference   {#mainpage}
========

@tableofcontents

The ScoreCleaner Audio Engine supports cross-platform audio, midi and signal processing.


Basic concepts
========

Signals and Processors
-------

The audio engine provides a high-level interface to audio processing based on *signals* and *processors*.
Signals may be though of as time-varying values, while processors may be thought of as functions from
signals to signals. Signals and processors are used to construct *Streams* on devices, or internal or
external buffers.


Audio signals and processors are represented by the [AudioSignal](@ref doremir::scl::AudioSignal) and
[AudioProcessor](@ref doremir::scl::AudioProcessor) classes respectively.

Time
-------

Time is represented as a number of samples with an optional fractional offset. This allows for precise conversions
between sample rates. Time is local to each stream computation, so two concurrent streams may not have a shared
notion of time. In each stream, time is monotonically increasing and time values are strictly ordered.


Streams
-------

In the context of the audio engine, the term *stream* represents an audio computation, in which values are streamed
from an input *source* to an output *sink*. They have no relation to the input and output streams of the standard
library (which are akin to sinks and sources, rather than streams).

Streams are represented by the [Stream](@ref doremir::scl::Stream) class.


Messages
-------

A *message* is an immutable, possibly nested list of primitive values. The audio engine uses
messages for all asynchonous communication, such as sending and receiving Midi, updating control values of audio
processors, or or waiting for results of non-realtime computations. In the documentation messages are written in a
Lisp-like syntax, i.e. `(:noteon 60 127)`.

Messages provide a dynamic interface which can be used as an alternative to the strongly typed methods. 
For example, an AudioProcessor may provide both a `gain(float x)` method, and accept messages on the 
form `(:gain ` *x* `)`.

Messages are represented by the [Message](@ref doremir::scl::Message) type. The [Sender](@ref doremir::scl::Sender)
and [Receiver](@ref doremir::scl::Receiver) interfaces are used to interact with the message passing system.

> **Note:** The Lisp API implicitly converts messages to Lisp lists and vice versa.

Future
------

A *future* represents the result of an asynchonous computation. Futures may be used to query the
result of a computation, wait for it to take place, interrupt it, or temporarily disable it. However,
they can not force the computation to take place.

Futures are represented by the [Future](@ref doremir::scl::Future) class.




Sessions and devices
========


Processors and signals
========

Real-time streams
========


Non-real-time streams
========


Utilities
========

Resource management
-------

Classes intended to be managed by the user implement the [Resource](@ref doremir::scl::Resource) interface, supporting reference counting and
automatic deallocation. Whenever an object implementing this interface is destructed, it will release its eventual references to other objects implementing that interface. Resources can be used with `boost::intrusive_ptr`


List processing
--------

As non-modified lists are used extensively in the audio engine, some routines for functional-style list processing
are provided in the [doremir::list](@ref doremir::scl::list) namespace. Allthough the standard library lists 
are mutable, the audio engine treats them as immutable.

### Construction

### Lifted algorithms

The following standard-library algorithms are provided as functions which automatically allocates new lists.

* replace
* replaceIf
* remove
* removeIf
* unique
* uniqueIf
* reverse
* reverseIf
* rotate
* partialSort
* merge
* intersection
* difference
* symmetricDifference
    
This makes it possible to write code such as.
    
    list<int> ys = list::replace(xs, 1, 2)
    
### Higher-order functions

* map
* foldMap
* foldLeft
* foldRight

### List predicates

The [doremir::list::predicate](@ref doremir::scl::list::predicate) namespace contains a handly DSL for specifying list predicates.
    
    list::create ( Or ( IsSymbol("noteon"), IsSymbol("noteoff") ), IsDouble(), IsDouble() )

