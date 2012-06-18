
Message passing
=====

The audio engine uses *message passing* to handle state transitions in the audio processing network. Messages are defined as immutable list of simple values, object references and list. The audio engine uses messages many forms of communication, such as sending and receiving notes, updating control values of audio processors, or or waiting for results of non-realtime computations. 

There are many similarities between messages and the native lists of languages such as Lisp. Indeed, the language bindings for the Audio Engine often converts messages to a high-level list structure in the target language and vice versa. It also use a Lisp-like syntax for documentation purposes. However, the messages used internally in the Audio Engine and their high-level language counterpart are not synonymous: the audio engine uses its own internal representation for messages and the translation is carried out by the language binding glue. Thus sending messages internally in the engine may be more efficient than passing them back and forth between the engine and another language environment.

The first member of such a list is often referred to as the *selector* of the message, allthough this is purely conventional: the first member may or may not have any specific significance.

> **Note:** The immutability property may or may not be enforced by the engine. Please do not mutate messages.

## Data types

## Senders and receivers

A *receiver* is the simplest type of message passing interface. From the owners point of view, it represents a sink into which the message is being fed; from an implementors point of view it represents a signature that describes an action to take whenever a message arrives. See [SclReceiver](@ref SclReceiver) and [Receiver](@ref doremir::scl::Receiver).

A *sender* is a point to which receivers may be attached. From the owners point of view, it represents a provider of messages with which a receiver may be registered; from an implementors point of view it represents a signature that handles registration and unregistration of receivers [SclSender](@ref SclSender) and [Sender](@ref doremir::scl::Sender).

## Broadcaster

A *broadcaster* is a simple synchronous sender, which forwards incoming messages to a set of registered receivers.

## Schedulers

A *scheduler* is an asynchronous sender, driven by an associated *time provider*. A scheduler acts as a broadcaster, but defers the delivery of messages until the time indicated together with the message. Schedulers provide *futures* which enable the sender of a message to interrupt or confirm its delivery. 


<!-- * Scheduled messages
* Notification messages
* Error messages -->