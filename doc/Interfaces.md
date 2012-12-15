
## Interfaces

The Audio Engine provides ad-hoc polymorphism using interfaces. An interface is a collection of methods designated by a unique interface identifier. Each type may provide an implementation for any number of interfaces.

Some basic interfaces include doremir_eq_t, doremir_ord_t and doremir_num_t.

By convention, the interface identifier is a global constant of the same name as the interface type, except for the `_t` suffix, so the identifier for the above interfaces are doremir_eq, doremir_ord and doremir_num respectively.


### Using an interface

Interface methods are called by invoking \ref doremir_get_interface, passing the value and the pointer instance. Note that this is the *only* way to call an interface method: in particular it is not safe to cast a pointer of some type to the interface type, even if the type happen to implement the interface.

For example, this is a way to implement the *min* function for any type supporing the \ref doremir_ord_t interface:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_ptr_t
doremir_min(doremir_ptr_t a, doremir_ptr_t b)
{             
    doremir_ord_t * ord = doremir_get_interface(doremir_ord, a);
    return ord->lt(a, b) ? a : b;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that most interfaces define wrappers for the the \ref doremir_get_interface call. By convention, the wrapper should be a function of the same name as the interface mehtod. This function is equivalent to the above definition:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doremir_ptr_t
doremir_min(doremir_ptr_t a, doremir_ptr_t b)
{
    return doremir_lt(a, b) ? a : b;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


As \ref doremir_get_interface returns a pointer to the interface or `null`, it can be used for dynamically inspecting a whether an arbitrary pointer supports an interface or not. If a type is known to support an
interface at compile-time, this check can be ommited.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bool has_eq(doremir_ptr a)
{
    return !!doremir_get_interface(doremir_eq, a);
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Defining an interface

To define a new interface, the following has to be provided:

* An interface struct
* An interface identifier

The struct is simply a typedef defining the types of the interface, for example

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

typedef struct {
            bool (* lt)(doremir_ptr_t, doremir_ptr_t);
            bool (* gt)(doremir_ptr_t, doremir_ptr_t);
        } doremir_ord_t;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


The easiest way to provide the identifier is to use \ref doremir_id.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
const doremir_id_t doremir_ord = doremir_id("doremir_ord");
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As described above, it is good style to also provide a global wrapper for each method:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inline bool doremir_lt (doremir_ptr_t, doremir_ptr_t)
{
    return doremir_get_interface(doremir_ord, a)->lt(a, b);
}
inline bool doremir_gt (doremir_ptr_t, doremir_ptr_t)
{
    return doremir_get_interface(doremir_ord, a)->gt(a, b);
}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



### Implementing an interface

To implement an interface for a pointer type, the following has to be provided:

* Functions implementing the interface methods
* A \ref doremir_impl_t lookup function

The lookup function is unique for each type, and performs a case matching on the
incoming interface identifiers, returning a pointer to the appropriate interface
struct.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bool
_foo_eq(doremir_ptr_t a, doremir_ptr_t b)
{
    return false;
}
bool
_foo_lt(doremir_ptr_t a, doremir_ptr_t b)
{
    return false;
}
bool
_foo_gt(doremir_ptr_t a, doremir_ptr_t b)
{
    return false;
}

doremir_ptr_t
foo_impl(doremir_id_t interface)
{
    static doremir_eq_t  foo_eq_impl  = { _foo_eq };
    static doremir_ord_t foo_ord_impl = { _foo_lt, _foo_gt };

    switch (interface)
    {
    case doremir_eq:
        return &foo_eq_impl;
    case doremir_ord:
        return &foo_ord_impl;
    default:
        return NULL;
    }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For \ref doremir_get_interface to work properly, the lookup function has to be the *first* element
of the type (i.e. have the same address as the type).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
struct _foo
{
    doremir_impl_t impl;
    ...
};

struct _foo * create_foo()
{
    struct _foo * = malloc(sizeof(_foo));
    foo->impl = &foo_impl;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                           