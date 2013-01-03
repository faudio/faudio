
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir.h>
#include <doremir/util.h>
#include <doremir/buffer.h>

struct _doremir_buffer_t {
        doremir_impl_t  impl;       /* Interface dispatcher */
        size_t size;
        uint8_t * data;
};

/**
    @typedef doremir_buffer_t
    The buffer.
 */

doremir_ptr_t buffer_impl(doremir_id_t interface);


/** Create a new buffer.
    @note
        O(n)
 */
doremir_buffer_t
doremir_buffer_create(size_t size)
{
    buffer_t b = doremir_new(buffer);
    b->impl = &buffer_impl;
    b->size = size;
    b->data = calloc(1, size);
    return b;
}

/** Copy the given buffer.
    @note
        O(n)
 */
doremir_buffer_t
doremir_buffer_copy(doremir_buffer_t buffer)
{
    return doremir_buffer_resize(buffer->size, buffer);
}

/** Copy the given buffer using the given size.
    @note
        O(n)
 */
doremir_buffer_t
doremir_buffer_resize(size_t size,
                      doremir_buffer_t buffer)
{
    buffer_t copy = doremir_new(buffer);
    copy->impl = &buffer_impl;
    copy->size = size;
    copy->data = malloc(size);
    copy->data = memcpy(copy->data, buffer->data, size);
    return copy;
}

/** Destroy the given buffer.
    @note
        O(n)
 */
void
doremir_buffer_destroy(doremir_buffer_t buffer)
{
    free(buffer->data);
    doremir_delete(buffer);
}

/** Return the size of the buffer.
    @note
        O(1)
 */
size_t
doremir_buffer_size(doremir_buffer_t buffer)
{
    return buffer->size;
}

/** Return the address of the buffer.

    This function is unsafe as it provides access to the buffer contents without
    ownership semantics.
    @note
        O(1)
 */
void * doremir_buffer_unsafe_address(doremir_buffer_t buffer)
{
    return buffer->data;
}


/** Read a value from the buffer.
    @note
        O(1)
 */
uint8_t
doremir_buffer_peek(doremir_buffer_t buffer, size_t index)
{
    return buffer->data[index];
}

/** Update a value in the buffer.
    @note
        O(1)
 */
void
doremir_buffer_poke(doremir_buffer_t buffer, size_t index, uint8_t value)
{
    buffer->data[index] = value;
}


// --------------------------------------------------------------------------------

doremir_ptr_t buffer_copy(doremir_ptr_t a)
{
    return doremir_buffer_copy(a);
}

void buffer_destroy(doremir_ptr_t a)
{
    doremir_buffer_destroy(a);
}

doremir_string_t buffer_show(doremir_ptr_t a)
{
    buffer_t buffer = (buffer_t) a;
    bool     more   = doremir_buffer_size(buffer) > 80;
    size_t   length = more ? 80 : doremir_buffer_size(buffer);
    string_t str    = string("<Buffer");

    // str = sdappend(str, string("["));
    // str = sdappend(str, doremir_string_format_integer("%i", length));
    // str = sdappend(str, string("]"));

    for (size_t i = 0; i < length; ++i)
    {
        str = sdappend(str, string(" "));
        str = sdappend(str, doremir_string_format_integer(
            "%02x",
            doremir_buffer_peek(buffer, i)));
    }
    if (more)
    {
        str = sdappend(str, string(" "));
        str = sdappend(str, string("..."));
    }

    str = sdappend(str, string(">"));
    return str;
}

doremir_ptr_t buffer_impl(doremir_id_t interface)
{
    static doremir_string_show_t buffer_show_impl = { buffer_show };
    static doremir_copy_t buffer_copy_impl = { buffer_copy };
    static doremir_destroy_t buffer_destroy_impl = { buffer_destroy };

    switch (interface)
    {
    case doremir_copy_i:
        return &buffer_copy_impl;

    case doremir_destroy_i:
        return &buffer_destroy_impl;

    case doremir_string_show_i:
        return &buffer_show_impl;

    default:
        return NULL;
    }
}

