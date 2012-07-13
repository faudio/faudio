Overview
==========

This page is a simple overview of the architecture and implementation of the audio engine.

The audio engine is implemented in C++ with bindings available for other languages. There is a C level API providing wrapper functions for C++ methods and classes. This allow the C++ API to be consumed by any language that interfaces with C.

Naming conventions
----------

In C++, every class and functions reside in the namespace `scl`. C functions and macros are prefixed with `scl` and `SCL` respectively. This should minimize conflicts with other libraries that may be present in the same library or excutable. Furthermore, the API is organized in several sub-namespaces of `scl`:

* `scl::base`       
  * Core classes, required by everything else.
* `scl::math`       
  * Math-related classes and functions.
* `scl::list`       
  * List-related classes and functions.
* `scl::thread`     
  * Thread- and concurrency-related classes and functions.
* `scl::util`       
  * Utility classes and functions, generally not dependent on anything else.


* `scl::audio::processor`
  * Provides creation and composition of audio processors. 
* `scl::audio::device`
  * Provides enumeration and management of real-time audio and midi-devices. 
* `scl::audio::scheduling`
  * Provides scheduling and communication.
* `scl::audio::stream`
  * Provides streams, which encapsulate the device, processor and scheduling functionality into an audio computation.
