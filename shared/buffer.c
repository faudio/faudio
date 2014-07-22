
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/buffer.h>
#include <fa/error.h>
#include <fa/util.h>

#include <sndfile.h>

/*
    ## Notes

    * Each buffer is an address range with an optional destroy function
      and its closure.

    * The create/destroy functions uses the standard fa_malloc/fa_free allocator.
 */

#define kMaxPrintSize 80
#define buffer_warn(str) // do nothing

struct _fa_buffer_t {
    fa_impl_t       impl;

    size_t          size;
    uint8_t         *data;

    fa_unary_t      destroy_function;
    fa_ptr_t        destroy_data;

    fa_map_t        meta;
};


void buffer_fatal(char *msg, int error);

fa_ptr_t default_destroy(fa_ptr_t _, fa_ptr_t data)
{
    fa_free(data);
    return NULL;
}

fa_buffer_t fa_buffer_create(size_t size)
{
    fa_ptr_t buffer_impl(fa_id_t interface);

    fa_buffer_t buffer = fa_new(buffer);

    buffer->impl = &buffer_impl;
    buffer->size = size;
    buffer->data = fa_malloc(size);

    buffer->destroy_function = default_destroy;
    buffer->destroy_data     = NULL;

    buffer->meta = fa_map_empty();

    memset(buffer->data, 0, buffer->size);

    if (!buffer->data) {
        if (errno == ENOMEM) {
            buffer_fatal("Out of memory", errno);
        } else {
            buffer_fatal("Unknown", errno);
        }
    }

    buffer_warn(fa_string("Buffer created"));
    return buffer;
}

fa_buffer_t fa_buffer_wrap(fa_ptr_t   pointer,
                           size_t      size,
                           fa_unary_t destroy_function,
                           fa_ptr_t   destroy_data)
{
    fa_ptr_t buffer_impl(fa_id_t interface);

    fa_buffer_t b = fa_new(buffer);
    b->impl = &buffer_impl;
    b->size = size;
    b->data = pointer;

    b->destroy_function = destroy_function;
    b->destroy_data     = destroy_data;

    b->meta = fa_map_empty();

    buffer_warn(fa_string("Buffer wrapped"));
    return b;
}

fa_buffer_t fa_buffer_copy(fa_buffer_t buffer)
{
    return fa_buffer_resize(buffer->size, buffer);
}

fa_buffer_t fa_buffer_resize(size_t size, fa_buffer_t buffer)
{
    fa_ptr_t buffer_impl(fa_id_t interface);

    fa_buffer_t copy           = fa_new(buffer);
    copy->impl              = &buffer_impl;
    copy->size              = size;
    copy->data              = fa_malloc(size);
    copy->destroy_function  = buffer->destroy_function;
    copy->destroy_data      = buffer->destroy_data;
    copy->meta              = fa_copy(buffer->meta);

    if (!copy->data) {
        if (errno == ENOMEM) {
            buffer_fatal("Out of memory", errno);
        } else {
            buffer_fatal("Unknown", errno);
        }
    }

    buffer_warn(fa_string("Buffer resized/copied"));

    copy->data = memcpy(copy->data, buffer->data, size);
    return copy;
}

fa_buffer_t fa_buffer_dresize(size_t size, fa_buffer_t buffer)
{
    // TODO could use realloc and be much more efficient

    fa_buffer_t buffer2 = fa_buffer_resize(size, buffer);
    fa_destroy(buffer);
    return buffer2;
}

void fa_buffer_destroy(fa_buffer_t buffer)
{
    if (buffer->destroy_function) {
        buffer->destroy_function(buffer->destroy_data, buffer->data);
    }

    // TODO recursive (everything in here is copy)
    fa_destroy(buffer->meta);

    buffer_warn(fa_string("Buffer destroyed"));
    fa_delete(buffer);
}

size_t fa_buffer_size(fa_buffer_t buffer)
{
    return buffer->size;
}

fa_ptr_t fa_buffer_get_meta(fa_buffer_t buffer, fa_string_t name)
{
    return fa_map_get(name, buffer->meta);
}

void fa_buffer_set_meta(fa_buffer_t buffer, fa_string_t name, fa_ptr_t value)
{
    buffer->meta = fa_map_dset(name, value, buffer->meta);
}

fa_map_t fa_buffer_meta(fa_buffer_t buffer)
{
    return buffer->meta;
}

uint8_t fa_buffer_get(fa_buffer_t buffer, size_t index)
{
    assert(index < buffer->size && "Buffer overflow");
    return buffer->data[index];
}

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


void *fa_buffer_unsafe_address(fa_buffer_t buffer)
{
    return buffer->data;
}

// --------------------------------------------------------------------------------

typedef fa_string_t path_t;

fa_buffer_t fa_buffer_read_audio(fa_string_t path)
{
    int             channels;
    fa_buffer_t        buffer;

    SNDFILE         *file;
    SF_INFO         info;
    info.format     = 0;

    {
        char *cpath     = fa_string_to_utf8(path);
        file            = sf_open(cpath, SFM_READ, &info);

        if (sf_error(file)) {
            char err[100];
            snprintf(err, 100, "Could not read audio file '%s'", cpath);
            return (fa_buffer_t) fa_error_create_simple(error, fa_string(err), fa_string("Doremir.Buffer"));
        }

        fa_inform(fa_string_append(fa_string("Reading "), path));
    }
    {
        size_t bufSize  = info.frames * info.channels * sizeof(double);
        buffer          = fa_buffer_create(bufSize);
        double *raw     = fa_buffer_unsafe_address(buffer);

        sf_count_t sz   = sf_read_double(file, raw, bufSize / sizeof(double));
        buffer          = fa_buffer_dresize(sz * sizeof(double), buffer);

        if (info.channels == 1) {
            channels = 1;
        } else if (info.channels == 2) {
            channels = 2;
        } else {
            buffer_fatal("Unknown buffer type", info.channels);
        }

        // Meta-data
        fa_buffer_set_meta(buffer, fa_string("sample-rate"), fa_f32(info.samplerate));
        fa_buffer_set_meta(buffer, fa_string("channels"), fa_i32(info.channels));
        {
            char *str = (char *) sf_get_string(file, SF_STR_TITLE);
            fa_buffer_set_meta(buffer, fa_string("title"), fa_string(str ? str : ""));
        }
        {
            char *str = (char *) sf_get_string(file, SF_STR_SOFTWARE);
            fa_buffer_set_meta(buffer, fa_string("software"), fa_string(str ? str : ""));
        }
        {
            char *str = (char *) sf_get_string(file, SF_STR_COPYRIGHT);
            fa_buffer_set_meta(buffer, fa_string("copyright"), fa_string(str ? str : ""));
        }

        if (sf_close(file)) {
            return (fa_buffer_t) fa_error_create_simple(error, fa_string("Could not close"), fa_string("Doremir.Buffer"));
        }
    }

    return buffer;
}

// TODO only writes one channel etc
fa_ptr_t fa_buffer_write_audio(fa_string_t  path,
                               fa_buffer_t  buffer)
{
    int channels = 1;
    assert(channels == 1 && "fa_buffer_write_audio: Can not write more than 1 channels");

    const char     *cpath = fa_string_to_utf8(path);
    double         *ptr   = fa_buffer_unsafe_address(buffer);
    size_t         size   = fa_buffer_size(buffer) / sizeof(double);


    SF_INFO         info;
    // FIXME other sample rates
    info.samplerate = 44100;
    info.channels   = 1;
    info.format     = SF_FORMAT_WAV | SF_FORMAT_PCM_24;
    SNDFILE        *file  = sf_open(cpath, SFM_WRITE, &info);

    if (sf_error(file)) {
        char err[100];
        snprintf(err, 100, "Could not write audio file '%s' (%s)", cpath, sf_strerror(file));
        return (fa_pair_t) fa_error_create_simple(
                   error, fa_string(err), fa_string("Doremir.Buffer"));
    }

    sf_count_t written = sf_write_double(file, ptr, size);

    if (written != size) {
        return (fa_pair_t) fa_error_create_simple(error, fa_string("To few bytes written"), fa_string("Doremir.Buffer"));
    }

    if (sf_close(file)) {
        return (fa_pair_t) fa_error_create_simple(error, fa_string("Could not close"), fa_string("Doremir.Buffer"));
    }

    return NULL;
}


int16_t fa_buffer_get_int16(fa_buffer_t x, size_t y)
{
    assert(false && "Not implemented");
}
int32_t fa_buffer_get_int32(fa_buffer_t x, size_t y)
{
    assert(false && "Not implemented");
}
int64_t fa_buffer_get_int64(fa_buffer_t x, size_t y)
{
    assert(false && "Not implemented");
}
void fa_buffer_set_int16(fa_buffer_t x, size_t y, int16_t z)
{
    assert(false && "Not implemented");
}
void fa_buffer_set_int32(fa_buffer_t x, size_t y, int32_t z)
{
    assert(false && "Not implemented");
}
void fa_buffer_set_int64(fa_buffer_t x, size_t y, int64_t z)
{
    assert(false && "Not implemented");
}
fa_buffer_t fa_buffer_read_raw(fa_string_t x)
{
    assert(false && "Not implemented");
}
void fa_buffer_write_raw(fa_string_t x, fa_buffer_t y)
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
    fa_buffer_t buffer = (fa_buffer_t) a;
    bool     more   = fa_buffer_size(buffer) > kMaxPrintSize;
    size_t   length = more ? kMaxPrintSize : fa_buffer_size(buffer);
    fa_string_t str    = fa_string("<Buffer");

    for (size_t i = 0; i < length; ++i) {
        str = fa_string_dappend(str, fa_string(" "));
        str = fa_string_dappend(str, fa_format_integral(
                                    "%02x",
                                    fa_buffer_get(buffer, i)));
    }

    if (more) {
        str = fa_string_dappend(str, fa_string(" "));
        str = fa_string_dappend(str, fa_string("..."));
    }

    str = fa_string_dappend(str, fa_string(">"));
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
    void fa_log_error_from(fa_string_t msg, fa_string_t origin);

    fa_log_error_from(fa_string_dappend(fa_string(msg), fa_format_integral(" (error code %d)", error)), fa_string("Doremir.Buffer"));
    fa_log_error(fa_string("Terminating Audio Engine"));
    exit(error);
}



