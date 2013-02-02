
# Events and scheduling {#Scheduling}

@anchor Scheduling
@tableofcontents

@note
    This page is under construction.

## Triggers vs Signals

* Semantic
    * t -> a vs (t,t) -> [a]
* Triggers handle *reference types* (like the data structures and queues)
* Signals only handle the *signal types*
* Triggers are run in schedulers
    * Activated through the loop() function
    * Runs in the main thread (or application thread)
    * Doing internal allocation/deallocation
    * Can run user callbacks, interact with the OS, check system time etc
* Signals are run in devices
    * Activated through run() or start()
    * Runs in a real-time thread (unless NRT)
    * All memory pre-allocated
    * Can only send/receive messages