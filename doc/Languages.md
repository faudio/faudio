
# Languages {#LanguageBindings}

@tableofcontents

## Lisp

### Conventions

Names are the same as in C except that:

- There is no global prefix, instead all symbols are in the `faudio` package.
- Names are separated by dashes instead of underscores.

For example @ref fa_audio_name becomes `(faudio:audio-name x)`.

Sometimes a Lisp wrapper is added to adapt error checking, wrapping of closures etc. The
name is the same as the wrapped function with a star suffix. For example, `list-find` is
available in two forms:

* `fa:list-find`     accepts CFFI callbacks
* `fa:list-find*`    accepts functions, callbacks and lambdas

    
### Types

Opaque types (most of them) are represented by a Lisp class of the same name. For example
the type String in module Fa.String is represented by a type `(defclass fa:string)`.

Generic functions work for most types, see the "Implements" section in each relevant
module documentation entry.

Note that there is no multiple dispatch in this system: implementations assume that the
given arguments are of the same type and dispatches on the leftmost argument. If you try
to compare, say an integer and a list you will crash.

### Resource management
    
This is manual. Each created value object must be destroyed by passing it to `(fa:destroy)`, or
another destructive function such as `(list-dcons)`. See the reference page Data Structures
for more information.

We might want to hook into the Lisp GC using something like the trivial garbage; however
in many cases this might not necessarily be practical. Things like sessions, devices and 
buffers often needs to be managed manually in any case (the same problem applies to streams
and sockets).
                                                              
### Undefined behaviour

  - Calling a function on a destroyed object is undefined
  - Calling (from-pointer) with the wrong type is undefined
  - Calling a generic function on a type that does not implement a required interface is undefined

~~~~
(setf x (*~ (constant 0.3) 
            (sine 220)))

(signal-run-file 
 (* 44100 60) 
 x 
 "/Users/hans/audio/out.wav") 
~~~~