
# Introduction {#mainpage}

The DoReMIR Audio Engine provides real-time and non-real-time audio processing,
midi, message passing and scheduling.  It can be used as a framework for 
developing audio related tools.

<!-- 
The Audio Engine represent audio computations using the concepts of *signals*
and *processors*, derived from [functional reactive programming][frp]. Signals
represent values over time and can be constructed out of primitives such as
constant, linear functions and so on. Processors represent functions from
signals to signals. Signal networks of any complexity can be described as a
processor or signal function.

The Audio Engine support stateful processors throught the use of *message
passing*. Processors in real-time streams can control processors in
non-real-time streams and vice versa by sending messages to each other. Messages
are transmitted via *dispatchers* that abstract away the complexity of
transporting messages over thread boundaries.

Messages are constructed from immutable collections and can be serialized as
[JSON][json] expressions.

The Audio Engine is specifically designed to interact well with *multiple client
languages*. This is achieved by specifying it in [Modulo][modulo] and implementing
it in [C][c99]. Typically, the Audio Engine is built as a shared library and linked
dynamically by the runtime of the client language. It is also possible to write
applications directly in C or C++.
 -->
 
© DoReMIR Music Research 2013. All rights reserved.


[frp]:      http://stackoverflow.com/questions/1028250/what-is-functional-reactive-programming
[modulo]:   https://github.com/hanshoglund/modulo
[c99]:      http://en.wikipedia.org/wiki/C99
[json]:     http://www.json.org/