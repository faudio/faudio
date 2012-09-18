
Futures and improving values
==========

A *future* (also called promise) is a handle to an asynchronous value of some type. The value
referred to by a future may be computed on another thread and may not yet be available. Any
thread holding a future may poll for the value, or block waiting for the value to be computed.
Once available, the future value will never change. Note that the underlying computation may run
forever, meaning that threads blocking on the future may never wake up.

The audio engine specifically uses *interruptible* and *interceptable* futures. These extend the
usual notion of a future with interruption semantics, giving the holder of the future a chance
to stop or pause the asynchronous computation. Interruptible futures are also used as
simple interrupts: in this case value of the computation is uninteresting (usually *null*).

An *improving value* is the continuous version of a future. While a future only have two states:
not available or available, an improving value generalizes this by proving a success
of *improving* states. The value in each state is required to be monotonically increasing.
Eventually, an improving value may become *fixed*, meaning that it will not improve further. A
thread holding an improving value may poll the current state, or block waiting for the value to
become fixed. As with futures, the underlying computation may run forever, in which case the
value will never become fixed.

\pagebreak

