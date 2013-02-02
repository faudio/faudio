
# Error handling {#Errors}

@anchor Errors
@tableofcontents

@note
    This page is under construction.

Error handling in the Audio Engine comes in three varieties:
  
  * User errors
  * Recoverable errors
  * Non-recoverable errors
  

# User errors {#UserErrors}

Program errors are those that are caused directly by the library user, often by
using a library function in the wrong way. Preferably all such errors should be
caught by the compiler, but this is not always possible.

Some examples of user errors are:

  * Calling @ref doremir_buffer_peek with an index outside the buffer range.
  * Calling @ref doremir_destroy on a value after passing it to a destructive function.
  * Passing a value missing a required interface to a generic function or collection.
  
User errors have undefined result, but in practice the process in which the Audio
Engine is running will probably crash. If the library is built in debug mode, an
file number and message is usually printed. 


# Recoverable errors {#Recoverable}

Recoverable errors are those that occur outside the control of the user, but in control
of the Audio Engine.

Generally such errors can be handled by the user by some special mechanism in the
library interface such as exceptions or status codes. In the Audio Engine,
recoverable errors always occur when a function is called, and must be detected by
the Program by inspecting the return value of the function. They are grouped into
*optional* values and *error* values.


## Optional values {#ErrorOpt}

Optional values simply means that a function returns null instead of an ordinary
value. They are used for simple cases where no additional information about the
condition is needed. Examples of functions returning optional values are
@ref doremir_list_index and @ref doremir_priority_queue_peek.


## Error values {#ErrorValues}

Error values are used in cases where the system has access to information about the
error. Error values depend on the interface mechanism: any value can be passed to
@ref doremir_error_check, which returns true if and only the value is an error.

Functions returning errors must have their return value passed to @ref
doremir_error_check before the value is used by another function. If an error has
occurred, check will return true and the other methods of the @ref doremir_error_t
interface can be used to obtain more information about the condition, otherwise the
value can be used normally. Note that values returned from construction and copy
functions must be destroyed whether an error has occured or not.


## Non-recoverable errors {#Nonrecoverable}

Non-recoverable errors are those that occur outside the control of the both library
user and the Audio Engine. They will usually log and terminate the process.


# Logging {#Logging}

The Audio Engine provides a simple global logging system. The user can add messages
to the log by calling one of the following functions. Non-recoverable errors are
added to the log automatically.

* [log](@ref doremir_audio_engine_log)
* [logInfo](@ref doremir_audio_engine_log_info)
* [logWarning](@ref doremir_audio_engine_log_warning)
* [logError](@ref doremir_audio_engine_log_error)

The default log function does nothing. The [Doremir.AudioEngine][dae] module
provides a function to replace this function, and some default implementations that
writes to the standard output, or to a specific log file.


[dae]: @ref DoremirAudioEngine