
# Error handling {#Errors}

@anchor Errors
@tableofcontents

Error handling in the Audio Engine comes in three varieties:

* User errors
* Recoverable errors
* Non-recoverable errors


# User errors {#User}

Program errors are those that are caused directly by the library user, often by
using a library function in the wrong way. Preferably all such errors should be
caught by the compiler, but this is not always possible.

Some examples of user errors are:

* Calling [peek](@ref doremir_buffer_poke) with an index outside the buffer range.
  * Correct usage: call [size](@ref doremir_buffer_size) to determine the size of
    the buffer.
* Passing a value missing a required interface to a generic function or collection.
  * Correct usage: Use a [dynamic check](@ref DynInterfaceCheck), an 
    [generic value](@ref Existential) or some other technique to assure that the value 
    implement all required interfaces.
* Calling [destroy](@ref doremir_destroy) on a value after passing it
  to a destructive function. 
  * Correct usage: Destroy the value exactly once.

User errors have undefined result, but in practice the process in which the Audio
Engine is running will probably crash. If the library is built in debug mode, an
file number and message is usually printed. Note that while user errors are
ubiquitous in a C or C++ library, they can often be avoided when using another
language. For example Lisp bindings could be written to always perform dynamic
interface checks, always call destroy from a finalizer and so on.


# Recoverable errors {#Recoverable}

Recoverable errors are those that occur outside the control of the user, but in control
of the Audio Engine.

Generally such errors can be handled by the Program by some special mechanism in the
library interface such as exceptions or status codes. In the Audio Engine,
recoverable errors always occur when a function is called, and must be detected by
the Program by inspecting the return value of the function. They are grouped into
*optional* values and *error* values.


## Optional values {#ErrorOpt}

Optional values simply means that a function returns `null` instead of an ordinary
value@small<sup>[1](@ref ErrorsNote1)</sup>. They are used for simple cases where
no additional information about the condition is needed. Examples of functions
returning optional values are [index](@ref doremir_list_index) and 
[peek](@ref doremir_priority_queue_peek).


## Error values {#ErrorValues}

Error values are used in cases where the system has access to information about the error. 
Error values depend on the interface mechanism: any value can be passed to 
[check](@ref doremir_check), which returns true if and only the value is an error.

Functions returning errors must have their return value passed to *check* before
the value is used by another function. If an error has occurred, check will return
true and the other methods of the [Error](@ref doremir_error_t) interface can be
used to obtain more information about the condition, otherwise the value can be
used normally. Note that values returned from construction and copy functions must
be destroyed whether an error has occured or not.


## Non-recoverable errors {#Nonrecoverable}

Non-recoverable errors are those that occur outside the control of the both library
user and the Audio Engine. They will usually log and terminate the process.


<!-- ### Errors and callbacks {#ErrorCallback}

TODO -->


# Logging {#Logging}

The Audio Engine provides a simple global logging system. All error values (not
optional values) will be passed to a global log function before they are returned
to the Program. By default, this function does nothing. The [audio engine](@ref DoremirAudioEngine) 
module provides a function to replace this function, and some default
implementations that writes to the standard output, or to a specific log file.

The program can add messages to the log by calling [log](@ref doremir_error_log), 
[logInfo](@ref doremir_error_log_info), [logWarning](@ref doremir_error_log_info) or
[logError](@ref doremir_error_log_info).


----------

@anchor ErrorsNote1

1. There are no functions returning `null` for any other purpose in the Audio
Engine. This implies that null can never be used in other contexts: in particular
it can not be passed to a queue or stored in a collection.
