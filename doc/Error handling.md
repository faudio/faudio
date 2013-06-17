
# Error handling {#id10048}

@anchor Errors
@tableofcontents

@note
    This page is under construction.


Generally, errors can be grouped into *recoverable* and *non-recoverable* errors.

Non-recoverable errors are those that occur outside the control of the Audio
Engine. They will usually terminate the process. Recoverable errors are those that
occur outside the control of the user, but in control of the Audio Engine. In most
systems, such errors can be *handled* by the user by some mechanism in the API such
as exceptions.

# Handling errors {#id441}

In the Audio Engine, recoverable errors always occur when a function is called, and
must be detected by the user by inspecting the return value of the function. They
are grouped into *optional* values and *error* values.

## Optional values {#id1152}

*Optional values* simply means that a function returns null instead of an ordinary
value. They are used for simple cases where no additional information about the
condition is needed. Examples of functions returning optional values are
ref fae_list_index and @ref fae_priority_queue_peek.

## Error values {#id24834}

*Error values* are used in cases where the system has access to information about the
error. Error values depend on the interface mechanism: any value can be passed to
@ref fae_check, which returns true if and only if the value is an error. 

Functions returning errors must have their return value passed to @ref fae_check 
before the value is used by another function. If an error has
occurred, check will return true and the other methods of the @ref fae_error_t
interface can be used to obtain more information about the condition, otherwise the
value can be used normally. Note that values returned from construction and copy
functions must be destroyed whether an error has occured or not.

<!--
TODO not sure about this...

As sa a special case in the interface mechanism, `null` is considered to implement
[Error](@ref fae_error_t). This means that the same procedure can be used to
check for optional values and error values.
-->


# Logging {#id24103}

The Audio Engine provides a simple logging system. 

## Setting up the log handler {#id30965}

By default, the Audio Engine discards all incoming log messages. To set a different
behaviour, use one of the setup functions in [Doremir.AudioEngine][dae]. Typically
you want to set the log handler to write to a file, or pass them to a custom
handler function.

## Adding log entries {#id10103}

Non-recoverable errors are always logged. The user can add recoverable errors to
the log using @ref fae_audio_engine_log. Typically, this function is used with
@ref fae_check, as in:

~~~
if (fae_check(value)) {
    fae_error_log(NULL, value);
    exit(-1);
}
~~~

There are also some convenience functions to log an arbitrary entry, for example:

~~~
fae_audio_engine_log_info("Are you aware of this?");
fae_audio_engine_log_error("That is an error!");
~~~


[dae]: @ref DoremirAudioEngine