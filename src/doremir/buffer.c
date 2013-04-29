
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/buffer.h>
#include <doremir/error.h>
#include <doremir/util.h>

#include <sndfile.h>

/*
    Notes:
 */

#define print_max_size_k 80

struct _doremir_buffer_t {
    impl_t          impl;       //  Interface dispatcher
    size_t          size;
    uint8_t        *data;
};


void buffer_fatal(char *msg, int error);

/** Create a new buffer.
    @note
        O(n)
 */
doremir_buffer_t doremir_buffer_create(size_t size)
{
    doremir_ptr_t buffer_impl(doremir_id_t interface);

    buffer_t b = doremir_new(buffer);
    b->impl = &buffer_impl;
    b->size = size;
    b->data = malloc(size);
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

/** Copy the given buffer.
    @note
        O(n)
 */
doremir_buffer_t doremir_buffer_copy(doremir_buffer_t buffer)
{
    return doremir_buffer_resize(buffer->size, buffer);
}

/** Copy the given buffer using the given size.
    @note
        O(n)
 */
doremir_buffer_t doremir_buffer_resize(size_t size, doremir_buffer_t buffer)
{
    doremir_ptr_t buffer_impl(doremir_id_t interface);

    buffer_t copy = doremir_new(buffer);
    copy->impl = &buffer_impl;
    copy->size = size;
    copy->data = malloc(size);

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
void doremir_buffer_destroy(doremir_buffer_t buffer)
{
    free(buffer->data);
    doremir_delete(buffer);
}

/** Return the size of the buffer.
    @note
        O(1)
 */
size_t doremir_buffer_size(doremir_buffer_t buffer)
{
    return buffer->size;
}

/** Read a value from the buffer.
    @note
        O(1)
 */
uint8_t doremir_buffer_get(doremir_buffer_t buffer, size_t index)
{
    assert(index < buffer->size && "Buffer overflow");
    return buffer->data[index];
}

/** Update a value in the buffer.
    @note
        O(1)
 */
void doremir_buffer_set(doremir_buffer_t buffer, size_t index, uint8_t value)
{
    assert(index < buffer->size && "Buffer overflow");
    buffer->data[index] = value;
}

float doremir_buffer_get_float(doremir_buffer_t buffer, size_t index)
{
    assert(index * sizeof(float) < buffer->size && "Buffer overflow");
    return ((float *) buffer->data)[index];
}

void doremir_buffer_set_float(doremir_buffer_t buffer, size_t index, float value)
{
    assert(index * sizeof(float) < buffer->size && "Buffer overflow");
    ((float *) buffer->data)[index] = value;
}

double doremir_buffer_get_double(doremir_buffer_t buffer, size_t index)
{
    assert(index * sizeof(double) < buffer->size && "Buffer overflow");
    return ((double *) buffer->data)[index];
}

void doremir_buffer_set_double(doremir_buffer_t buffer, size_t index, double value)
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
void *doremir_buffer_unsafe_address(doremir_buffer_t buffer)
{
    return buffer->data;
}

// --------------------------------------------------------------------------------

typedef doremir_string_file_path_t path_t;

doremir_pair_t doremir_buffer_read_audio(doremir_string_file_path_t path)
{
    type_t type;
    buffer_t buffer;

    SF_INFO info;
    info.format = 0;
    char *file = doremir_string_to_utf8(path);
    SNDFILE *f = sf_open(file, SFM_READ, &info);

    if (sf_error(f)) {
        char err[100];
        snprintf(err, 100, "Could not read audio file '%s'", file);
        return (pair_t) doremir_error_create_simple(error, string(err), string("Doremir.Buffer"));
    }

    inform(string_dappend(string("Reading "), string(file)));

    size_t bufSize = info.frames * info.channels * sizeof(double);
    buffer = doremir_buffer_create(bufSize);
    double *raw = doremir_buffer_unsafe_address(buffer);

    sf_count_t sz = sf_read_double(f, raw, bufSize / sizeof(double));
    buffer = doremir_buffer_resize(sz * sizeof(double), buffer);

    if (info.channels == 1) {
        type = type_vector(type(f64), info.frames);
    } else if (info.channels == 2) {
        type = type_vector(type_pair(type(f64), type(f64)), info.frames);
    } else {
        buffer_fatal("Unknown buffer type", info.channels);
    }

    return pair(type, buffer);
}

void doremir_buffer_write_audio(doremir_string_file_path_t path,
                                doremir_type_t             type,
                                doremir_buffer_t           buffer)
{
    assert(false && "Not implemented");
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
    bool     more   = doremir_buffer_size(buffer) > print_max_size_k;
    size_t   length = more ? print_max_size_k : doremir_buffer_size(buffer);
    string_t str    = string("<Buffer");

    for (size_t i = 0; i < length; ++i) {
        str = string_dappend(str, string(" "));
        str = string_dappend(str, format_integral(
                                 "%02x",
                                 doremir_buffer_get(buffer, i)));
    }

    if (more) {
        str = string_dappend(str, string(" "));
        str = string_dappend(str, string("..."));
    }

    str = string_dappend(str, string(">"));
    return str;
}

doremir_ptr_t buffer_impl(doremir_id_t interface)
{
    static doremir_string_show_t buffer_show_impl = { buffer_show };
    static doremir_copy_t buffer_copy_impl = { buffer_copy };
    static doremir_destroy_t buffer_destroy_impl = { buffer_destroy };

    switch (interface) {
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


void doremir_audio_engine_log_error_from(doremir_string_t msg, doremir_string_t origin);

void buffer_fatal(char *msg, int error)
{
    doremir_audio_engine_log_error_from(string_dappend(string(msg), format_integral(" (error code %d)", error)), string("Doremir.Buffer"));
    doremir_audio_engine_log_error(string("Terminating Audio Engine"));
    exit(error);
}



