    
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/buffer.h>
#include <fae/error.h>
#include <fae/util.h>

#include <sndfile.h>

/*
    Notes:
 */

#define print_max_size_k 80

struct _fae_buffer_t {
    impl_t          impl;               //  Interface dispatcher

    size_t          size;
    uint8_t        *data;

    fae_unary_t     destroy_function;
    fae_ptr_t       destroy_data;
};


void buffer_fatal(char *msg, int error);

fae_ptr_t default_destroy(fae_ptr_t b, fae_ptr_t _)
{
    free(b);
    return NULL;
}

/** Create a new buffer.
    @note
        O(n)
 */
fae_buffer_t fae_buffer_create(size_t size)
{
    fae_ptr_t buffer_impl(fae_id_t interface);

    buffer_t b = fae_new(buffer);
    b->impl = &buffer_impl;
    b->size = size;
    b->data = malloc(size);
    b->destroy_function = default_destroy;
    b->destroy_data     = NULL;
    memset(b->data, 0, b->size);

    if (!b->data) {
        if (errno == ENOMEM) {
            buffer_fatal("Out of memory", errno);
        } else {
            buffer_fatal("Unknown", errno);
        }
    }

    return b;
}

/** Create a buffer wrapping the given memory region.
    @param ptr  Pointer to wrap.
    @param size Number of bytes to wrap.
    @param destroy_function Function to call upon destruction (nullable).
    @param destroy_data Data closed over by the destroy function.
    @note
        O(1)
 */
fae_buffer_t fae_buffer_wrap(fae_ptr_t   ptr,
                             size_t      size,
                             fae_unary_t destroy_function,
                             fae_ptr_t   destroy_data)
{
    fae_ptr_t buffer_impl(fae_id_t interface);

    buffer_t b = fae_new(buffer);
    b->impl = &buffer_impl;
    b->size = size;
    b->data = ptr;
    b->destroy_function = destroy_function;
    b->destroy_data     = destroy_data;
    return b;
}

/** Copy the given buffer.
    @note
        O(n)
 */
fae_buffer_t fae_buffer_copy(fae_buffer_t buffer)
{
    return fae_buffer_resize(buffer->size, buffer);
}

/** Copy the given buffer using the given size.
    @note
        O(n)
 */
fae_buffer_t fae_buffer_resize(size_t size, fae_buffer_t buffer)
{
    fae_ptr_t buffer_impl(fae_id_t interface);

    buffer_t copy = fae_new(buffer);
    copy->impl = &buffer_impl;
    copy->size = size;
    copy->data = malloc(size);
    copy->destroy_function = buffer->destroy_function;
    copy->destroy_data     = buffer->destroy_data;

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
void fae_buffer_destroy(fae_buffer_t buffer)
{
    if (buffer->destroy_function) {
        buffer->destroy_function(buffer->data, buffer->destroy_data);
    }
    fae_delete(buffer);
}

/** Return the size of the buffer.
    @note
        O(1)
 */
size_t fae_buffer_size(fae_buffer_t buffer)
{
    return buffer->size;
}

/** Read a value from the buffer.
    @note
        O(1)
 */
uint8_t fae_buffer_get(fae_buffer_t buffer, size_t index)
{
    assert(index < buffer->size && "Buffer overflow");
    return buffer->data[index];
}

/** Update a value in the buffer.
    @note
        O(1)
 */
void fae_buffer_set(fae_buffer_t buffer, size_t index, uint8_t value)
{
    assert(index < buffer->size && "Buffer overflow");
    buffer->data[index] = value;
}

float fae_buffer_get_float(fae_buffer_t buffer, size_t index)
{
    assert(index * sizeof(float) < buffer->size && "Buffer overflow");
    return ((float *) buffer->data)[index];
}

void fae_buffer_set_float(fae_buffer_t buffer, size_t index, float value)
{
    assert(index * sizeof(float) < buffer->size && "Buffer overflow");
    ((float *) buffer->data)[index] = value;
}

double fae_buffer_get_double(fae_buffer_t buffer, size_t index)
{
    assert(index * sizeof(double) < buffer->size && "Buffer overflow");
    return ((double *) buffer->data)[index];
}

void fae_buffer_set_double(fae_buffer_t buffer, size_t index, double value)
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
void *fae_buffer_unsafe_address(fae_buffer_t buffer)
{
    return buffer->data;
}

// --------------------------------------------------------------------------------

typedef fae_string_file_path_t path_t;

fae_pair_t fae_buffer_read_audio(fae_string_file_path_t path)
{
    type_t type;
    buffer_t buffer;

    SF_INFO info;
    info.format = 0;
    char *file = fae_string_to_utf8(path);
    SNDFILE *f = sf_open(file, SFM_READ, &info);

    if (sf_error(f)) {
        char err[100];
        snprintf(err, 100, "Could not read audio file '%s'", file);
        return (pair_t) fae_error_create_simple(error, string(err), string("Doremir.Buffer"));
    }

    inform(string_dappend(string("Reading "), string(file)));

    size_t bufSize = info.frames * info.channels * sizeof(double);
    buffer = fae_buffer_create(bufSize);
    double *raw = fae_buffer_unsafe_address(buffer);

    sf_count_t sz = sf_read_double(f, raw, bufSize / sizeof(double));
    buffer = fae_buffer_resize(sz * sizeof(double), buffer);

    if (info.channels == 1) {
        type = type_vector(type(f64), info.frames);
    } else if (info.channels == 2) {
        type = type_vector(type_pair(type(f64), type(f64)), info.frames);
    } else {
        buffer_fatal("Unknown buffer type", info.channels);
    }

    return pair(type, buffer);
}

void fae_buffer_write_audio(fae_string_file_path_t path,
                                fae_type_t             type,
                                fae_buffer_t           buffer)
{
    assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

fae_ptr_t buffer_copy(fae_ptr_t a)
{
    return fae_buffer_copy(a);
}

void buffer_destroy(fae_ptr_t a)
{
    fae_buffer_destroy(a);
}

fae_string_t buffer_show(fae_ptr_t a)
{
    buffer_t buffer = (buffer_t) a;
    bool     more   = fae_buffer_size(buffer) > print_max_size_k;
    size_t   length = more ? print_max_size_k : fae_buffer_size(buffer);
    string_t str    = string("<Buffer");

    for (size_t i = 0; i < length; ++i) {
        str = string_dappend(str, string(" "));
        str = string_dappend(str, format_integral(
                                 "%02x",
                                 fae_buffer_get(buffer, i)));
    }

    if (more) {
        str = string_dappend(str, string(" "));
        str = string_dappend(str, string("..."));
    }

    str = string_dappend(str, string(">"));
    return str;
}

fae_ptr_t buffer_impl(fae_id_t interface)
{
    static fae_string_show_t buffer_show_impl = { buffer_show };
    static fae_copy_t buffer_copy_impl = { buffer_copy };
    static fae_destroy_t buffer_destroy_impl = { buffer_destroy };

    switch (interface) {
    case fae_copy_i:
        return &buffer_copy_impl;

    case fae_destroy_i:
        return &buffer_destroy_impl;

    case fae_string_show_i:
        return &buffer_show_impl;

    default:
        return NULL;
    }
}


void fae_fae_log_error_from(fae_string_t msg, fae_string_t origin);

void buffer_fatal(char *msg, int error)
{
    fae_fae_log_error_from(string_dappend(string(msg), format_integral(" (error code %d)", error)), string("Doremir.Buffer"));
    fae_fae_log_error(string("Terminating Audio Engine"));
    exit(error);
}



