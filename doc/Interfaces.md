
# Interfaces {#Interfaces}

@anchor Interfaces
@tableofcontents

@note
    This page is under construction.

In the context of the Audio Engine, an *interface* (not to be confuseed with and
*audio interace*) is a collection of function types, identified by a unique value
known as the *interface identifier*. They are used extensively inside the Audio
Engine.

Any [reference type][reftype] may provide implementations for an arbitrary number
of interfaces by implementing a so-called *dispatch function*, which takes a
reference of the given type and an interface identifier and returns a pointer to a
structure conforming to the interface type. This structure is known as an
*implementation*.

Interfaces can be used to decorate a type with additional semantics such as
[equality](@ref fae_equal_t) or [ordering](@ref fae_order_t). Another use
is to overload common functionality, such as 
[arithmetic operators](@ref fae_number_t).



# Using an interface {#Using}

Interface methods are called by invoking [interface](@ref fae_interface), passing the
interface identifier and the value on which the interface is going to be
dispatched. This is usually one of the arguments to the invoked method, but it can
be any value. If the given value does not implement the interface, @ref
[interface](@ref fae_interface) returns null.

Note that [interface](@ref fae_interface) is actually the *only* way to call an
interface method: in particular it is not safe to cast a pointer of some type to
the interface type and call the methods from that pointer. It follows that you must
not use a pointer to an interface type (such as [Equal](@ref fae_equal_t)) as
an argument to [interface](@ref fae_interface).


## Generic functions {#GenericFunctions}

Interfaces are commonly used to implement generic functions, which are functions
using an interface method without knowledge of the exact type. Generic functions
generally accept one or more parameters of type `void *`.

For example, this is a way to implement the *min* function for any type supporing
the [Order](@ref fae_order_t) interface.

~~~~
void * fae_min(void *a, void *b) 
{             
    return fae_interface(fae_order_i, a)->less_than(a, b) ? a : b;
}
~~~~

Note that most interfaces define generic functions wrapping their methods, saving
the user from having to write an explicit [interface](@ref fae_interface) call. By
convention, the wrapper should be a function of the same name as the interface
method. Thus the above function could be defined more briefly as follows.

~~~~
void * fae_min(void *a, void *b)
{
    return fae_less_than(a, b) ? a : b;
}
~~~~

Note that the restriction on arguments to generic functions correspond to
*universal* quantification (i.e. it says *for any type* a *such that* a *implements
the equal interface*).

## Generic values {#GenericPointers}

TODO

Note that restriction on generic values correspond to *existential* quantification
(i.e. it says *for some type* a *such that* a *implements the equal interface*).


## Dynamic checks {#DynInterfaceCheck}

As [interface](@ref fae_interface) returns a pointer to the interface or `null`, it can be
used for dynamically inspecting a whether an arbitrary value supports an interface
or not. If a type is known to support an interface at compile-time, this check can
be omitted.

This is a way to implement a safe equality check, which compares using the equal
interface if the given value implements it, and compares their addresses otherwise.

~~~~
bool safe_equal(void *a, void *b)
{       
    fae_equal_t *equal = fae_interface(fae_equal_i, a);

    if (equal)
        return equal->equal(a, b);
    else
        return a == b;
}
~~~~


# Defining an interface {#Defining}

To define a new interface, the following has to be provided:

* An interface struct
* An interface identifier

The struct is simply a typedef defining the types of the interface, for example

~~~~
typedef struct {

    bool (* less_than)(void *, void *);

    bool (* greater_than)(void *, void *);

} fae_order_t;
~~~~

The identifier should be defined as a macro or enum constant defining a unique
number.

~~~~
enum { fae_order_i = 255; };
~~~~

As described above, it is good style to also provide a generic function wrapping
each method:

~~~~
inline bool fae_less_than (void *, void *)
{
    return fae_interface(fae_order_i, a)->less_than(a, b);
}

inline bool fae_greater_than (void *, void *)
{
    return fae_interface(fae_order_i, a)->greater_than(a, b);
}
~~~~


# Implementing an interface {#Implementing}

To implement an interface for a reference type, the following has to be provided:

* Functions implementing the interface methods
* A dispatch function of type [Impl](@ref fae_impl_t)
* A field in the type that points to the dispatch function
* A construction routine that sets the pointer to the dispatch function

The dispatch function is unique for each type, and performs a case matching on the
incoming interface identifiers, returning a pointer to the appropriate interface
struct.

As an example, let us write a custom reference type `foo`, implementing @ref
fae_equal_t and [Order](@ref fae_order_t).

## The methods {#Methods}

The methods are written as ordinary functions, which have the same type as the
functions declared in the interface struct. These functions does need not be
exported.

~~~~
bool foo_equal(void *a, void *b)
{
    ...
}

bool foo_less_than(void *a, void *b)
{
    ...
}

bool foo_greater_than(void *a, void *b)
{
    ...
}
~~~~

## The dispatch function {#Dispatch}

The dispatch function should have the type [Impl](@ref fae_impl_t). For example:

~~~~
fae_ptr_t foo_impl(fae_id_t interface)
{
    static fae_equal_t foo_equal_impl = { foo_equal };
    static fae_order_t foo_order_impl = { foo_less_than, foo_greater_than };

    switch (interface)
    {
    case fae_equal_i:
        return &foo_equal_impl;

    case fae_order_i:
        return &foo_order_impl;

    default:
        return NULL;
    }
}
~~~~

## The interface pointer {#Pointer}

The address of the dispatch function has to be the *first* element of the
implementing type. The name of the fields is irrelevant, typically `impl` is used.

~~~~
struct foo
{
    fae_impl_t impl; /* Interface dispatcher* /
    ...
};
~~~~

The creation routine for the type should include a line to set up the `impl` field
to the address of the dispatch function. Note that a forward declaration might be
necessary here.

~~~~
fae_ptr_t foo_impl(fae_id_t interface);

struct foo *create_foo()
{
    struct foo *foo = malloc(sizeof(_foo));
    foo->impl = &foo_impl;                      /* Setting up dispatcher */
    ...
    return foo;
}
~~~~

[ad-hoc-poly]: http://en.wikipedia.org/wiki/Ad-hoc_polymorphism
[reftype]:     http://en.wikipedia.org/wiki/Reference_type