
# Events and scheduling {#Scheduling}

@anchor Events
@anchor Scheduling
@tableofcontents

@note
    This page is under construction.

## Events vs Signals

* Semantic
    * t -> a vs (t,t) -> [a]
* Events handle *reference types* (like the data structures and queues)
* Signals only handle the *signal types*
* Events are run in schedulers
    * Activated through the loop() function
    * Runs in the main thread (or application thread)
    * Doing internal allocation/deallocation
    * Can run user callbacks, interact with the OS, check system time etc
* Signals are run in devices
    * Activated through run() or start()
    * Runs in a real-time thread (unless NRT)
    * All memory pre-allocated
    * Can only send/receive messages