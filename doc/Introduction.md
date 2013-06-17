
Introduction {#mainpage}
====

The DoReMIR Audio Engine provides real-time and non-real-time audio processing.

## Basic concepts

* A *signal* is a time-varying value
* A *processor* is a transformer of signals
* A *device* is a provider and consumer of signals

* An *event* is a sequence of messages
* A *sender* is a provider of messages
* A *receiver* is a consumer of messages


### Signals:     

* Constant value                            (S a)
* Time (linear)                             S Time
* Map/Ap2                                   (a -> b) -> S a -> S b, (a -> b -> c) -> S a -> S b -> S c
* Accum/loop                                (Time -> a -> b -> b) -> S a -> S b
* Read/write buffer                         B a -> S a, B a -> S a -> S ()
* Send/recv to bus                          Z a -> S a, B a -> S a -> S ()
* Switch from messages                      S a -> E (S a) -> S a
* External: VST, AU, FluidSynth etc         S a -> S b

### Events:
