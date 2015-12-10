
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/buffer.h>
#include <fa/error.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/atomic.h>

#include <sndfile.h>

/*
    ## Notes

    * Each buffer is an address range with an optional destroy function
      and its closure.

    * The create/destroy functions uses the standard fa_malloc/fa_free allocator.

    * Buffers have a reference count, modified with @ref fa_buffer_take_reference and
      @ref fa_buffer_release_reference. If the reference count is not zero,
      fa_destroy only marks for future destruction, and the
      real destruction happens when the reference count reaches zero.
    
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
    
    fa_atomic_t     ref_count;
    fa_atomic_t     marked_for_destruction;
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
    buffer->ref_count = fa_atomic();
    buffer->marked_for_destruction = fa_atomic();
    
    //fa_slog_info("fa_buffer_create ", fa_i32(size));

    buffer->destroy_function = default_destroy;
    buffer->destroy_data     = NULL;

    buffer->meta = fa_map_empty();
    fa_map_set_value_destructor(buffer->meta, fa_destroy);

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
                           size_t     size,
                           fa_unary_t destroy_function,
                           fa_ptr_t   destroy_data)
{
    fa_ptr_t buffer_impl(fa_id_t interface);

    fa_buffer_t b = fa_new(buffer);
    b->impl = &buffer_impl;
    b->size = size;
    b->data = pointer;
    b->ref_count = fa_atomic();
    b->marked_for_destruction = fa_atomic();

    b->destroy_function = destroy_function;
    b->destroy_data     = destroy_data;

    b->meta = fa_map_empty();

    buffer_warn(fa_string("Buffer wrapped"));
    return b;
}

fa_buffer_t fa_buffer_copy(fa_buffer_t buffer)
{
    assert(false && "Not implemented");
    return fa_buffer_resize(buffer->size, buffer);
}

fa_buffer_t fa_buffer_resize(size_t size, fa_buffer_t buffer)
{
    assert(false && "Not implemented");
//     fa_ptr_t buffer_impl(fa_id_t interface);
//
//     fa_buffer_t copy        = fa_new(buffer);
//     copy->impl              = &buffer_impl;
//     copy->size              = size;
//     copy->data              = fa_malloc(size);
//     copy->destroy_function  = buffer->destroy_function;
//     copy->destroy_data      = buffer->destroy_data;
//     copy->meta              = fa_deep_copy(buffer->meta);
//
//     if (!copy->data) {
//         if (errno == ENOMEM) {
//             buffer_fatal("Out of memory", errno);
//         } else {
//             buffer_fatal("Unknown", errno);
//         }
//     }
//
//     buffer_warn(fa_string("Buffer resized/copied"));
//
//     copy->data = memcpy(copy->data, buffer->data, size);
//     return copy;
}

fa_buffer_t fa_buffer_dresize(size_t size, fa_buffer_t buffer)
{
    //fa_slog_info("fa_buffer_dresize");

    buffer->data = fa_realloc(buffer->data, size);
    if (buffer->data) {
        buffer->size = size;
    } else {
        buffer->size = 0;
    }
    return buffer;
}

fa_pair_t fa_buffer_unzip(fa_buffer_t buffer)
{
    size_t sz = fa_buffer_size(buffer);
    fa_buffer_t buffer1 = fa_buffer_create(sz/2);
    fa_buffer_t buffer2 = fa_buffer_create(sz/2);
    
    for (int i = 0; i < (sz/2); ++i)
    {
        buffer1->data[i] = buffer->data[i*2+0];
    }
    for (int i = 0; i < (sz/2); ++i)
    {
        buffer2->data[i] = buffer->data[i*2+1];
    }    
    return fa_pair_create(buffer1, buffer2);
}

#define SIZE_MIN(x, y) (x < y ? x : y)

fa_buffer_t fa_buffer_zip(fa_buffer_t buffer1, fa_buffer_t buffer2)
{
    size_t sz = SIZE_MIN(fa_buffer_size(buffer1), fa_buffer_size(buffer2));
    fa_buffer_t buffer = fa_buffer_create(sz*2);

    for (int i = 0; i < sz; ++i)
    {
        buffer->data[i*2+0] = buffer1->data[i];
        buffer->data[i*2+1] = buffer2->data[i];
    }
    return buffer;
}

inline static
void resample_raw
    (
    double   old_rate,
    double   new_rate,
    double*  in_samples,
    size_t   in_sample_count,
    double*  out_samples,
    size_t   out_sample_count
    )
{
    // TODO
}

fa_buffer_t fa_buffer_resample_mono(double new_rate, fa_buffer_t buffer)
{
    size_t old_size = fa_buffer_size(buffer);
    double old_rate = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("sample-rate")));
    size_t new_size = ((double) old_size) * (new_rate / old_rate);

    fa_buffer_t buffer2 = fa_buffer_create(new_size);

    resample_raw(old_rate, new_rate, (double*) buffer->data, old_size / sizeof(double), (double*) buffer2->data, new_size / sizeof(double));

    buffer2->destroy_function   = buffer->destroy_function;
    buffer2->destroy_data       = buffer->destroy_data;
    buffer2->meta               = fa_copy(buffer->meta);
    fa_buffer_set_meta(buffer, fa_string("sample-rate"), fa_from_int32(new_rate));

    return buffer2;
}

fa_buffer_t fa_buffer_resample_stereo(double new_rate, fa_buffer_t buffer)
{
    fa_unpair(fa_buffer_unzip(buffer), b1, b2) {
        return fa_buffer_zip(fa_buffer_resample_mono(new_rate, b1), fa_buffer_resample_mono(new_rate, b2));
    }
    assert(false);
}

fa_buffer_t fa_buffer_resample(double new_rate, fa_buffer_t buffer)
{
    int ch = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("channels")));
    if (ch == 1) {
        return fa_buffer_resample_mono(new_rate, buffer);
    }
    if (ch == 2) {
        return fa_buffer_resample_stereo(new_rate, buffer);
    }
    fa_fail(fa_string("Buffer.resample: Unexpected number of channels: needs 1 or 2"));
    assert(false);
}

static inline void do_destroy_buffer(fa_buffer_t buffer)
{
    //fa_slog_info("do_destroy_buffer");
    if (buffer->destroy_function) {
        buffer->destroy_function(buffer->destroy_data, buffer->data);
    }

    fa_destroy(buffer->meta); // keys and values are automatically destroyed because the destructor is set

    buffer_warn(fa_string("Buffer destroyed"));
    fa_destroy(buffer->ref_count);
    fa_destroy(buffer->marked_for_destruction);
    fa_delete(buffer);
}

void fa_buffer_destroy(fa_buffer_t buffer)
{
    //fa_slog_info("fa_buffer_destroy");
    if ((size_t)fa_atomic_get(buffer->ref_count) == 0) {
        do_destroy_buffer(buffer);
    } else {
        fa_atomic_set(buffer->marked_for_destruction, (fa_ptr_t) true);
    }
}

void fa_buffer_take_reference(fa_buffer_t buffer)
{
    fa_atomic_add(buffer->ref_count, 1);
}

void fa_buffer_release_reference(fa_buffer_t buffer)
{
    fa_atomic_add(buffer->ref_count, -1);
    // TODO: is a lock needed?
    if ((size_t)fa_atomic_get(buffer->ref_count) == 0 && (bool)fa_atomic_get(buffer->marked_for_destruction)) {
        do_destroy_buffer(buffer);
    }
}

size_t fa_buffer_size(fa_buffer_t buffer)
{
    return buffer->size;
}

fa_ptr_t fa_buffer_get_meta(fa_buffer_t buffer, fa_string_t name)
{
    return fa_map_dget(name, buffer->meta);
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

#define BUFFER_PRIM_GET_SET(NAME,TYPE) \
    TYPE fa_buffer_get_##NAME(fa_buffer_t buffer, size_t index)                     \
    {                                                                               \
        if (index * sizeof(TYPE) >= buffer->size) { \
            printf("overflow, %zu >= %zu\n", index, buffer->size); \
            return 0; \
        } \
        return ((TYPE *) buffer->data)[index];                                      \
    }                                                                               \
                                                                                    \
    void fa_buffer_set_##NAME(fa_buffer_t buffer, size_t index, TYPE value)         \
    {                                                                               \
        assert(index * sizeof(TYPE) < buffer->size && "Buffer overflow");           \
        ((TYPE *) buffer->data)[index] = value;                                     \
    }                                                                               \
 
BUFFER_PRIM_GET_SET(bool, bool)
BUFFER_PRIM_GET_SET(float, float)
BUFFER_PRIM_GET_SET(double, double)
BUFFER_PRIM_GET_SET(int16, int16_t)
BUFFER_PRIM_GET_SET(int32, int32_t)
BUFFER_PRIM_GET_SET(int64, int64_t)

void *fa_buffer_unsafe_address(fa_buffer_t buffer)
{
    return buffer->data;
}

// --------------------------------------------------------------------------------

//typedef fa_string_t path_t;

fa_buffer_t fa_buffer_read_audio(fa_string_t path)
{
    fa_buffer_t     buffer;

    SNDFILE         *file;
    SF_INFO         info;
    info.format     = 0;

    {
        char *cpath     = fa_string_to_utf8(path);
        file            = sf_open(cpath, SFM_READ, &info);

        if (sf_error(file)) {
            char err[200];
            snprintf(err, 200, "Could not read audio file '%s'", cpath);
            return (fa_buffer_t) fa_error_create_simple(error, fa_string(err), fa_string("Doremir.Buffer"));
        }

        fa_inform(fa_string_dappend(fa_string("Reading "), fa_copy(path)));
    }
    {
        size_t bufSize  = info.frames * info.channels * sizeof(double);
        buffer          = fa_buffer_create(bufSize);
        double *raw     = fa_buffer_unsafe_address(buffer);

        sf_count_t sz   = sf_read_double(file, raw, bufSize / sizeof(double));
        buffer          = fa_buffer_dresize(sz * sizeof(double), buffer);

        // Meta-data

        fa_buffer_set_meta(buffer, fa_string("sample-rate"), fa_i32(info.samplerate));
        fa_buffer_set_meta(buffer, fa_string("channels"), fa_i32(info.channels));
        fa_buffer_set_meta(buffer, fa_string("format"), fa_i32(info.format));

        fa_let(str, (char *) sf_get_string(file, SF_STR_TITLE))
        fa_buffer_set_meta(buffer, fa_string("title"), fa_string(str ? str : ""));

        fa_let(str, (char *) sf_get_string(file, SF_STR_SOFTWARE))
        fa_buffer_set_meta(buffer, fa_string("software"), fa_string(str ? str : ""));

        fa_let(str, (char *) sf_get_string(file, SF_STR_COPYRIGHT))
        fa_buffer_set_meta(buffer, fa_string("copyright"), fa_string(str ? str : ""));

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
    const char     *cpath = fa_string_to_utf8(path);
    double         *ptr   = fa_buffer_unsafe_address(buffer);
    size_t         size   = fa_buffer_size(buffer) / sizeof(double);

    SF_INFO         info;

    info.samplerate = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("sample-rate")));
    info.channels   = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("channels")));
    info.format     = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("format")));

    SNDFILE        *file = sf_open(cpath, SFM_WRITE, &info);

    if (sf_error(file)) {
        char err[100];
        snprintf(err, 100, "Could not write audio file '%s' (%s)", cpath, sf_strerror(file));
        return fa_error_create_simple(error, fa_string(err), fa_string("Doremir.Buffer"));
    }

    sf_count_t written = sf_write_double(file, ptr, size);

    if (written != size) {
        return fa_error_create_simple(error, fa_string("To few bytes written"), fa_string("Doremir.Buffer"));
    }

    if (sf_close(file)) {
        return fa_error_create_simple(error, fa_string("Could not close"), fa_string("Doremir.Buffer"));
    }

    return NULL;
}

fa_buffer_t fa_buffer_read_raw(fa_string_t path)
{
    char* path2 = fa_string_to_utf8(path);
    FILE* file = fopen(path2, "rb");
    fa_free(path2);
    
    if (!file) {
        fa_warn(fa_dappend(fa_string("Could not open "), fa_copy(path)));
        return (fa_buffer_t) fa_error_create_simple(error, fa_string("Could not open file"), fa_string("Doremir.Buffer"));
    }

    // Get length of file
    fseek(file, 0, SEEK_END);
    long filelen = ftell(file);
    rewind(file);

    // Read entire file
    uint8_t *buffer = fa_malloc(filelen);
    fread(buffer, filelen, 1, file);
    fclose(file);
    
    return fa_buffer_wrap(buffer, filelen, default_destroy, NULL);
}

bool fa_buffer_write_raw(fa_string_t path, fa_buffer_t buffer)
{
    char* path2 = fa_string_to_utf8(path);
    FILE* file = fopen(path2, "wb");
    fa_free(path2);
    if (file) {
        fwrite(fa_buffer_unsafe_address(buffer), fa_buffer_size(buffer), 1, file);
        fclose(file);
        return true;
    } else {
        return false;
    }
}


// --------------------------------------------------------------------------------

fa_ptr_t buffer_copy(fa_ptr_t a)
{
    return fa_buffer_copy(a);
}

fa_ptr_t buffer_deep_copy(fa_ptr_t a)
{
    assert(false && "Not implemented");
}

void buffer_destroy(fa_ptr_t a)
{
    fa_buffer_destroy(a);
}

void buffer_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    if (p(a)) fa_buffer_destroy(a);
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

fa_dynamic_type_repr_t buffer_get_type(fa_ptr_t a)
{
    return buffer_type_repr;
}

fa_ptr_t buffer_impl(fa_id_t interface)
{
    static fa_string_show_t buffer_show_impl = { buffer_show };
    static fa_copy_t buffer_copy_impl = { buffer_copy, buffer_deep_copy };
    static fa_destroy_t buffer_destroy_impl = { buffer_destroy, buffer_deep_destroy };
    static fa_dynamic_t buffer_dynamic_impl = { buffer_get_type };

    switch (interface) {
    case fa_copy_i:
        return &buffer_copy_impl;

    case fa_destroy_i:
        return &buffer_destroy_impl;

    case fa_string_show_i:
        return &buffer_show_impl;
        
    case fa_dynamic_i:
        return &buffer_dynamic_impl;

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



