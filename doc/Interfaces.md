
## Interfaces

The Audio Engine provides ad-hoc polymorphism using interfaces. An interface is a collection of functions
designated by a unique interface identifier. Each type may provide an implementation for any number of
interfaces by implementing a dispatch function. Functions implementing an interface are conventionally
known as *methods*, while functions making use of the interface mechanism are called *generic* functions.

Some basic interfaces include doremir_equal_t, doremir_order_t and doremir_number_t.


### Using an interface

Interface methods are called by invoking \ref doremir_interface, passing the value and the value on which
the method is going to be dispatched. 

Note that under the hood, \ref doremir_interface is the *only* way to call a method: it is not safe to
cast a pointer of some type to the interface type regardless of whether the type implements the interface
or not.

For example, this is a way to implement the *min* function for any type supporing the \ref doremir_order_t
interface.

~~~~
doremir_ptr_t doremir_min(doremir_ptr_t a, 
                          doremir_ptr_t b) 
{             
    return doremir_interface(doremir_order_i, a)->less_than(a, b) ? a : b;
}
~~~~

Note that most interfaces define wrappers for the the \ref doremir_interface call. By convention, the
wrapper should be a function of the same name as the interface method. This function is equivalent to the
above definition.

~~~~
doremir_ptr_t doremir_min(doremir_ptr_t a, 
                          doremir_ptr_t b)
{
    return doremir_less_than(a, b) ? a : b;
}
~~~~

As \ref doremir_interface returns a pointer to the interface or `null`, it can be used for dynamically
inspecting a whether an arbitrary pointer supports an interface or not. If a type is known to support an
interface at compile-time, this check can be omitted.

~~~~
bool has_equality(doremir_ptr a)
{
    return doremir_interface(doremir_equal_i, a);
}
~~~~


### Defining an interface

To define a new interface, the following has to be provided:

* An interface struct
* An interface identifier

The struct is simply a typedef defining the types of the interface, for example

~~~~
typedef struct {

    bool (* less_than)(doremir_ptr_t, doremir_ptr_t);

    bool (* greater_than)(doremir_ptr_t, doremir_ptr_t);

} doremir_order_t;
~~~~

The identifier should be defined as a macro or enum constant defining a unique number.

~~~~
#define doremir_order_i 255;
~~~~

As described above, it is good style to also provide a global wrapper for each method:

~~~~
inline bool doremir_less_than (doremir_ptr_t, doremir_ptr_t)
{
    return doremir_interface(doremir_order_i, a)->less_than(a, b);
}

inline bool doremir_greater_than (doremir_ptr_t, doremir_ptr_t)
{
    return doremir_interface(doremir_order_i, a)->greater_than(a, b);
}

~~~~


### Implementing an interface

To implement an interface for a pointer type, the following has to be provided:

* Functions implementing the interface methods
* A dispatch function of type \ref doremir_impl_t
* A construction routine that sets the pointer to the dispatch function

The dispatch function is unique for each type, and performs a case matching on the
incoming interface identifiers, returning a pointer to the appropriate interface
struct. 

For \ref doremir_interface to work properly, the dispatch function has to be the *first* element of the
type (i.e. have the same address as the type).

~~~~
bool foo_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    return false;
}

bool foo_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    return false;
}

bool foo_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    return false;
}

doremir_ptr_t foo_impl(doremir_id_t interface)
{
    static doremir_equal_t foo_equal_impl = { foo_equal };
    static doremir_order_t foo_order_impl = { foo_less_than, foo_greater_than };

    switch (interface)
    {
    case doremir_equal_i:
        return &foo_equal_impl;

    case doremir_order_i:
        return &foo_order_impl;

    default:
        return NULL;
    }
}

struct foo
{
    doremir_impl_t impl;
    ...
};

struct foo *create_foo()
{
    struct foo *ptr = malloc(sizeof(_foo));
    ptr->impl = &foo_impl;
    ...
    return ptr;
}

void destroy_foo(struct foo* ptr)
{
    ...
    free(ptr);
}
~~~~

