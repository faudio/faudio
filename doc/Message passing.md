Message passing
=====

The audio engine uses *message passing* to handle state transitions in the processing network. Messages are defined as immutable list of simple values, object references and list. The audio engine uses messages many forms of communication, such as sending and receiving notes, updating control values of audio processors, or or waiting for results of non-realtime computations.

## Data types

## Senders and receivers

A [Receiver](@ref doremir::scl::Receiver) is the simplest type of message passing interface. From the user's point of view, it represents a sink into which the message is being fed; from the implementor's point of view it represents a signature that describes an action to take whenever a message arrives.

A [Sender](@ref doremir::scl::Sender) is a point to which receivers may be attached. From the user's point of view, it represents a provider of messages with which a receiver may be registered; from an implementor's point of view it represents a signature that handles registration and unregistration of receivers..

A [Broadcaster](@ref doremir::scl::Broadcaster) is a simple synchronous sender, which forwards incoming messages to a set of registered receivers.

A [Scheduler](@ref doremir::scl::Scheduler) is an asynchronous sender, driven by an associated [TimeProvider](@ref doremir::scl::TimeProvider). A scheduler acts as a broadcaster, but defers the delivery of messages until the time indicated together with the message. Schedulers may provide a [Future](@ref doremir::scl::Future) interface to enable the sender of a message to interrupt or confirm its delivery. 


<!-- * Scheduled messages
* Notification messages
* Error messages -->

