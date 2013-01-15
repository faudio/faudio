
# Introduction {#mainpage}

The DoReMIR Audio Engine provides real-time and non-real-time audio processing. 
It can be used as a library, or as a framework for developing audio related tools.

## Signals and processors

The Audio Engine represent audio computations using the concepts of *signals* and *processors*, derived
from [functional reactive programming](http://stackoverflow.com/questions/1028250/what-is-functional-reactive-programming).
Signals represent values over time and can be constructed out of primitives such as constant, linear functions
and so on. Processors represent functions from signals to signals. Signal networks of any complexity can
be described as a processor or signal function.

## Messages

The Audio Engine support stateful processors throught the use of *message passing*. Processors in real-time
streams can control processors in non-real-time streams and vice versa by sending messages to each other.
Messages are transmitted via *dispatchers* that use lock-free queues to transport messages over threads. 
Messages are constructed from immutable collections and can be serialized as [JSON](http://www.json.org/) expressions.

## Implementation and usage patterns

The Audio Engine is specified using the [Modulo](https://github.com/hanshoglund/modulo) description language
and implemented in [C](http://en.wikipedia.org/wiki/C99). Modulo can generate bindings to a host of other
languages, including Lisp. Typically, the Audio Engine is built as a shared library and linked dynamically
by the runtime of the client language. It is also possible to write applications directly in C, using static
or dynamic linkage.

@note 
    This documentation is still under construction. 
    Please reports errors and omissions to <hans.hoglund@doremir.com>.

@author 
    Hans Höglund

@copyright 
    DoReMIR Music Research 2012. All rights reserved.
