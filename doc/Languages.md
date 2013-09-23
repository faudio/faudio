
# Languages {#LanguageBindings}

@tableofcontents

# Lisp {#Lisp}

## Conventions of this thing {#Conventionsofthisthing}

Names are the same as in C except that:

- There is no global prefix, instead all symbols are in the `faudio` package.
- Names are separated by dashes instead of underscores.

For example @ref fa_audio_name becomes `(faudio:audio-name x)`.

Sometimes a Lisp wrapper is added to adapt error checking, wrapping of closures etc. The
name is the same as the wrapped function with a star suffix. For example, `list-find` is
available in two forms:

* `fa:list-find`     accepts CFFI callbacks
* `fa:list-find*`    accepts functions, callbacks and lambdas

Non-primitive types are represented by a Lisp class of the same name. For example
the type String in module [Fa.Signal](@ref FaSignal) is represented by a type `(defclass fa:signal)`.

## Resource management {#Resourcemanagement}
    
This is manual. Each created value object must be destroyed by passing it to
`(fa:destroy)`, or another destructive function such as `(list-dcons)`. See the reference
page [Data Structures](@ref DataStructures) for more information.
                                                              
## Undefined behaviour {#Undefinedbehaviour}

  - Calling a function on a destroyed object is undefined
  - Calling (from-pointer) with the wrong type is undefined
  - Calling a generic function on a type that does not implement a required interface is undefined

~~~~
(setf x (*~ (constant 0.3) 
            (sine 220)))
                
; Generate 60 seconds of audio
(signal-run-file (* 44100 60) x "/Users/hans/audio/out.wav") 
~~~~