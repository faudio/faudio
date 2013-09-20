
/*
    FA
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fa/buffer.h>
#include <fa/error.h>
#include <fa/util.h>

#include <sndfile.h>

/*
    Notes:
 */

#define print_max_size_k 80

struct _fa_buffer_t {
    impl_t          impl;

    size_t          size;
    uint8_t        *data;

    fa_unary_t     destroy_function;
    fa_ptr_t       destroy_data;
};


void buffer_fatal(char *msg, int error);

fa_ptr_t default_destroy(fa_ptr_t b, fa_ptr_t _)
{
    free(b);
    return NULL;
}

/** Create a new buffer.
    @note
        O(n)
 */
fa_buffer_t fa_buffer_create(size_t size)
{
    fa_ptr_t buffer_impl(fa_id_t interface);

    buffer_t buffer = fa_new(buffer);

    buffer->impl = &buffer_impl;
    buffer->size = size;
    buffer->data = malloc(size);

    buffer->destroy_function = default_destroy;
    buffer->destroy_data     = NULL;

    memset(buffer->data, 0, buffer->size);

    if (!buffer->data) {
        if (errno == ENOMEM) {
            buffer_fatal("Out of memory", errno);
        } else {
            buffer_fatal("Unknown", errno);
        }
    }

    return buffer;
}

/** Create a buffer wrapping the given memory region.

    @param ptr  Pointer to wrap.
    @param size Number of bytes to wrap.
    @param destroy_function Function to call upon destruction (nullable).
    @param destroy_data Data closed over by the destroy function.
    @note
        O(1)
 */
fa_buffer_t fa_buffer_wrap(fa_ptr_t   pointer,
                             size_t      size,
                             fa_unary_t destroy_function,
                             fa_ptr_t   destroy_data)
{
    fa_ptr_t buffer_impl(fa_id_t interface);

    buffer_t b = fa_new(buffer);
    b->impl = &buffer_impl;
    b->size = size;
    b->data = pointer;

    b->destroy_function = destroy_function;
    b->destroy_data     = destroy_data;

    return b;
}

/** Copy the given buffer.
    @note
        O(n)
 */
fa_buffer_t fa_buffer_copy(fa_buffer_t buffer)
{
    return fa_buffer_resize(buffer->size, buffer);
}

/** Copy the given buffer using the given size.
    @note
        O(n)
 */
fa_buffer_t fa_buffer_resize(size_t size, fa_buffer_t buffer)
{
    fa_ptr_t buffer_impl(fa_id_t interface);

    buffer_t copy           = fa_new(buffer);
    copy->impl              = &buffer_impl;
    copy->size              = size;
    copy->data              = malloc(size);
    copy->destroy_function  = buffer->destroy_function;
    copy->destroy_data      = buffer->destroy_data;

    if (!copy->data) {
        if (errno == ENOMEM) {
            buffer_fatal("Out of memory", errno);
        } else {
            buffer_fatal("Unknown", errno);
        }
    }

    copy->data = memcpy(copy->data, buffer->data, size);
    return copy;
}

/** Destroy the given buffer.
    @note
        O(n)
 */
void fa_buffer_destroy(fa_buffer_t buffer)
{
    if (buffer->destroy_function) {
        buffer->destroy_function(buffer->data, buffer->destroy_data);
    }

    fa_delete(buffer);
}

/** Return the size of the buffer.
    @note
        O(1)
 */
size_t fa_buffer_size(fa_buffer_t buffer)
{
    return buffer->size;
}

/** Read a value from the buffer.
    @note
        O(1)
 */
uint8_t fa_buffer_get(fa_buffer_t buffer, size_t index)
{
    assert(index < buffer->size && "Buffer overflow");
    return buffer->data[index];
}

/** Update a value in the buffer.
    @note
        O(1)
 */
void fa_buffer_set(fa_buffer_t buffer, size_t index, uint8_t value)
{
    assert(index < buffer->size && "Buffer overflow");

    buffer->data[index] = value;
}

float fa_buffer_get_float(fa_buffer_t buffer, size_t index)
{
    assert(index * sizeof(float) < buffer->size && "Buffer overflow");

    return ((float *) buffer->data)[index];
}

void fa_buffer_set_float(fa_buffer_t buffer, size_t index, float value)
{
    assert(index * sizeof(float) < buffer->size && "Buffer overflow");

    ((float *) buffer->data)[index] = value;
}

double fa_buffer_get_double(fa_buffer_t buffer, size_t index)
{
    assert(index * sizeof(double) < buffer->size && "Buffer overflow");

    return ((double *) buffer->data)[index];
}

void fa_buffer_set_double(fa_buffer_t buffer, size_t index, double value)
{
    assert(index * sizeof(double) < buffer->size && "Buffer overflow");

    ((double *) buffer->data)[index] = value;
}


/** Return the address of the buffer.

    This function is unsafe as it provides access to the buffer contents without
    ownership semantics.
    @note
        O(1)
 */
void *fa_buffer_unsafe_address(fa_buffer_t buffer)
{
    return buffer->data;
}

// --------------------------------------------------------------------------------

typedef fa_string_file_path_t path_t;

/**
    Reads and audio file.

    @returns
        A pair $$type, buffer)$$.
 */
fa_pair_t fa_buffer_read_audio(fa_string_file_path_t path)
{
    type_t          type;
    buffer_t        buffer;

    SNDFILE         *file;
    SF_INFO         info;
    info.format     = 0;

    {
        char *cpath     = fa_string_to_utf8(path);
        file            = sf_open(cpath, SFM_READ, &info);

        if (sf_error(file)) {
            char err[100];
            snprintf(err, 100, "Could not read audio file '%s'", cpath);
            return (pair_t) fa_error_create_simple(
                       error, string(err), string("Doremir.Buffer"));
        }
        inform(string_append(string("Reading "), path));
    }
    {
        size_t bufSize  = info.frames * info.channels * sizeof(double);
        buffer          = fa_buffer_create(bufSize);
        double *raw     = fa_buffer_unsafe_address(buffer);

        sf_count_t sz   = sf_read_double(file, raw, bufSize / sizeof(double));
        buffer          = fa_buffer_resize(sz * sizeof(double), buffer);

        if (info.channels == 1) {
            type = type_vector(type(f64), info.frames);
        } else if (info.channels == 2) {
            type = type_vector(type_pair(type(f64), type(f64)), info.frames);
        } else {
            buffer_fatal("Unknown buffer type", info.channels);
        }
    }
    return pair(type, buffer);
}

void fa_buffer_write_audio(fa_string_file_path_t path,
                            fa_type_t             type,
                            fa_buffer_t           buffer)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

fa_ptr_t buffer_copy(fa_ptr_t a)
{
    return fa_buffer_copy(a);
}

void buffer_destroy(fa_ptr_t a)
{
    fa_buffer_destroy(a);
}

fa_string_t buffer_show(fa_ptr_t a)
{
    buffer_t buffer = (buffer_t) a;
    bool     more   = fa_buffer_size(buffer) > print_max_size_k;
    size_t   length = more ? print_max_size_k : fa_buffer_size(buffer);
    string_t str    = string("<Buffer");

    for (size_t i = 0; i < length; ++i) {
        str = string_dappend(str, string(" "));
        str = string_dappend(str, format_integral(
                                 "%02x",
                                 fa_buffer_get(buffer, i)));
    }

    if (more) {
        str = string_dappend(str, string(" "));
        str = string_dappend(str, string("..."));
    }

    str = string_dappend(str, string(">"));
    return str;
}

fa_ptr_t buffer_impl(fa_id_t interface)
{
    static fa_string_show_t buffer_show_impl = { buffer_show };
    static fa_copy_t buffer_copy_impl = { buffer_copy };
    static fa_destroy_t buffer_destroy_impl = { buffer_destroy };

    switch (interface) {
    case fa_copy_i:
        return &buffer_copy_impl;

    case fa_destroy_i:
        return &buffer_destroy_impl;

    case fa_string_show_i:
        return &buffer_show_impl;

    default:
        return NULL;
    }
}


void buffer_fatal(char *msg, int error)
{
    void fa_fa_log_error_from(fa_string_t msg, fa_string_t origin);

    fa_fa_log_error_from(string_dappend(string(msg), format_integral(" (error code %d)", error)), string("Doremir.Buffer"));
    fa_fa_log_error(string("Terminating Audio Engine"));
    exit(error);
}



