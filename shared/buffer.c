
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
#if _WIN32
SNDFILE* sf_wchar_open (const wchar_t *wpath, int mode, SF_INFO *sfinfo); // See note in io.c
#endif

// #include <mpg123.h>


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

fa_buffer_t fa_buffer_dwrap(fa_ptr_t pointer, size_t size)
{
    //printf("fa_buffer_dwrap  pointer: %p, size: %zu\n", pointer, size);
    assert(pointer && size && "Cannot wrap a NULL pointer!");
    return fa_buffer_wrap(pointer, size, default_destroy, NULL);
}

fa_buffer_t fa_buffer_copy(fa_buffer_t buffer)
{
    assert(false && "fa_buffer_copy not implemented");
    return fa_buffer_resize(buffer->size, buffer);
}

fa_buffer_t fa_buffer_resize(size_t size, fa_buffer_t buffer)
{
    assert(false && "fa_buffer_resize not implemented");
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
    return fa_buffer_read_audio_max_size(path, 0, false);
}

fa_buffer_t fa_buffer_read_audio_max_size(fa_string_t path, size_t max_size, bool crop)
{
    fa_buffer_t     buffer;

    SNDFILE         *file;
    SF_INFO         info;
    info.format     = 0;
    bool cropped    = false;

    {
        #if _WIN32
        wchar_t *cpath  = fa_string_to_utf16(path);
        file            = sf_wchar_open(cpath, SFM_READ, &info);
        #else
        char *cpath     = fa_string_to_utf8(path);
        file            = sf_open(cpath, SFM_READ, &info);
        #endif

        if (sf_error(file)) {
            char err[200];
            snprintf(err, 200, "Could not read audio file '%s'", cpath);
            return (fa_buffer_t) fa_error_create_simple(error, fa_string(err), fa_string("Doremir.Buffer"));
        }

        fa_inform(fa_string_dappend(fa_string("Reading "), fa_copy(path)));
    }
    {
        size_t bufSize  = info.frames * info.channels * sizeof(double);
        
        // Required size is too big
        if (max_size && bufSize > max_size) {
            if (crop) {
                bufSize = max_size;
                cropped = true;
            } else {
                fa_slog_warning("Audio file too big");
                return NULL;
            }
        }
        
        buffer          = fa_buffer_create(bufSize);
        double *raw     = fa_buffer_unsafe_address(buffer);

        sf_count_t sz   = sf_read_double(file, raw, bufSize / sizeof(double));
        buffer          = fa_buffer_dresize(sz * sizeof(double), buffer);

        // Meta-data

        fa_buffer_set_meta(buffer, fa_string("sample-rate"), fa_i32(info.samplerate));
        fa_buffer_set_meta(buffer, fa_string("channels"), fa_i32(info.channels));
        fa_buffer_set_meta(buffer, fa_string("format"), fa_i32(info.format));
        fa_buffer_set_meta(buffer, fa_string("cropped"), fa_from_bool(cropped));
        fa_buffer_set_meta(buffer, fa_string("frames"), fa_i64(sz / info.channels));

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
    double         *ptr   = fa_buffer_unsafe_address(buffer);
    size_t         size   = fa_buffer_size(buffer) / sizeof(double);

    SF_INFO        info;

    info.samplerate = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("sample-rate")));
    info.channels   = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("channels")));
    info.format     = fa_peek_int32(fa_buffer_get_meta(buffer, fa_string("format")));

    #if _WIN32
    wchar_t *cpath  = fa_string_to_utf16(path);
    SNDFILE *file   = sf_wchar_open(cpath, SFM_WRITE, &info);
    #else
    char *cpath     = fa_string_to_utf8(path);
    SNDFILE *file   = sf_open(cpath, SFM_WRITE, &info);
    #endif

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

fa_buffer_t fa_buffer_read_raw_max_size(fa_string_t path, size_t max_size)
{
    char* path2 = fa_string_to_utf8(path);
    FILE* file = fa_fopen(path2, "rb");
    fa_free(path2);
    
    if (!file) {
        fa_warn(fa_dappend(fa_string("Could not open "), fa_copy(path)));
        return (fa_buffer_t) fa_error_create_simple(error, fa_string("Could not open file"), fa_string("Doremir.Buffer"));
    }

    // Get length of file
    fseek(file, 0, SEEK_END);
    long filelen = ftell(file);
    rewind(file);
    
    if (max_size && filelen > max_size) {
        return NULL;
    }

    // Read entire file
    uint8_t *buffer = fa_malloc(filelen);
    fread(buffer, filelen, 1, file);
    fclose(file);
    
    return fa_buffer_wrap(buffer, filelen, default_destroy, NULL);
}

fa_buffer_t fa_buffer_read_raw(fa_string_t path)
{
    return fa_buffer_read_raw_max_size(path, 0);
}

bool fa_buffer_write_raw(fa_string_t path, fa_buffer_t buffer)
{
    char* path2 = fa_string_to_utf8(path);
    FILE* file = fa_fopen(path2, "wb");
    fa_free(path2);
    if (file) {
        fwrite(fa_buffer_unsafe_address(buffer), fa_buffer_size(buffer), 1, file);
        fclose(file);
        return true;
    } else {
        return false;
    }
}

/*
fa_buffer_t fa_buffer_read_mp3(fa_string_t path)
{
    return fa_buffer_read_mp3_max_size(path, 0, false);
}

static fa_buffer_t mpg123_error_from_code(int code) {
    fa_string_t error_string = fa_format("mpg123 error: %s", mpg123_plain_strerror(code));
	fa_fail(fa_copy(error_string));
	return (fa_buffer_t) fa_error_create_simple(error, error_string, fa_string("Doremir.Buffer"));
}

static fa_buffer_t mpg123_error(mpg123_handle *handle) {
    fa_string_t error_string = fa_format("mpg123 error: %s", mpg123_strerror(handle));
    if (handle) mpg123_delete(handle);
	fa_fail(fa_copy(error_string));
	return (fa_buffer_t) fa_error_create_simple(error, error_string, fa_string("Doremir.Buffer"));
}

fa_buffer_t fa_buffer_read_mp3_max_size(fa_string_t path, size_t max_size, bool crop)
{
    if (crop) {
        return (fa_buffer_t) fa_error_create_simple(error,
            fa_string("crop not yet supported for mp3"), fa_string("Doremir.Buffer"));
    }
    fa_buffer_t buffer;
    int error;
    mpg123_handle *mp3 = mpg123_new(NULL, &error);
	if (mp3 == NULL) return mpg123_error_from_code(error);
    mpg123_param(mp3, MPG123_ADD_FLAGS, MPG123_FORCE_FLOAT, 0);
    
    {
        const long *rates;
        size_t rate_count;
        mpg123_format_none(mp3);
        mpg123_rates(&rates, &rate_count);
        for (int i = 0; i < rate_count; i++) {
            mpg123_format(mp3, rates[i], MPG123_MONO|MPG123_STEREO, MPG123_ENC_FLOAT_32);
            mpg123_format(mp3, rates[i], MPG123_MONO|MPG123_STEREO, MPG123_ENC_FLOAT_64);
        }
    }

    bool cropped = false;
	int channels = 0;
	int encoding = 0;
	long sample_rate = 0;
    char *cpath = fa_string_to_utf8(path);
    if (mpg123_open(mp3, cpath) || mpg123_getformat(mp3, &sample_rate, &channels, &encoding)) {
        fa_free(cpath);
        return mpg123_error(mp3);
    }
    fa_free(cpath);
    int sample_size = MPG123_SAMPLESIZE(encoding);
    //int frame_size = sample_size * channels;
    
    fa_inform(fa_string_dappend(fa_string("Reading mp3 from "), fa_copy(path)));
    fa_inform(fa_format("sample_rate: %ld  channels: %d  encoding: %d", sample_rate, channels, encoding));
    //fa_inform(fa_format("sample_size: %d  frame_size: %d  samples (expected): %lld", sample_size, frame_size, mpg123_length(mp3)));

    mpg123_scan(mp3);
    size_t bufSize = mpg123_length(mp3) * channels * sizeof(double);
    
    //fa_inform(fa_format("bufSize: %zu bytes (max: %zu)", bufSize, max_size));
        
    // Required size is too big
    if (max_size && bufSize > max_size) {
        if (crop) {
            bufSize = max_size;
            cropped = true;
        } else {
            fa_slog_warning("Mp3 file too big");
            mpg123_close(mp3);
            mpg123_delete(mp3);
            return NULL;
        }
    }
    
    buffer = fa_buffer_create(bufSize); // round(bufSize * 1.1));
    double *raw = fa_buffer_unsafe_address(buffer);
    size_t pos = 0;
    bool done = false;
    
    int read_buffer_size = mpg123_outblock(mp3);
	unsigned char *read_buffer = fa_malloc(read_buffer_size);
    
    do {
        size_t bytes_read;
        error = mpg123_read(mp3, read_buffer, read_buffer_size, &bytes_read);
        size_t samples_read = bytes_read / sample_size;
        
        //fa_inform(fa_format("pos: %zu  samples_read: %zu", pos, samples_read));
        if (encoding == MPG123_ENC_FLOAT_32) {
            for (int i = 0; i < samples_read; i++) {
                raw[pos+i] = ((float*)read_buffer)[i];
            }
        } else if (encoding == MPG123_ENC_FLOAT_64) {
            //memcpy(raw+pos, read_buffer, bytes_read); // Wrong: pos is not bytes!
            fa_slog_error("MPG123_ENC_FLOAT_64 not implemented");
            assert(false && "MPG123_ENC_FLOAT_64 not supported");
        } else {
            assert(false && "Bad sample format (this should never happen)");
        }
        pos += samples_read;
        
        switch (error) {
            case MPG123_NEW_FORMAT: fa_warn(fa_format("new format, bytes_read: %zu", bytes_read)); break;
            case MPG123_DONE:       done=1;break;//fa_inform(fa_format("done, bytes_read: %zu", bytes_read)); done = 1; break;
            case MPG123_OK:         break;//fa_inform(fa_format("ok, bytes_read: %zu", bytes_read)); break;
            default: {
                mpg123_close(mp3);
                mpg123_delete(mp3);
                fa_buffer_destroy(buffer);
                fa_free(read_buffer);
                return mpg123_error(mp3);
            }
        }
    } while (!done);
    
    fa_free(read_buffer);
    mpg123_close(mp3);
    mpg123_delete(mp3);

    // Meta-data
    fa_buffer_set_meta(buffer, fa_string("sample-rate"), fa_i32(sample_rate));
    fa_buffer_set_meta(buffer, fa_string("channels"), fa_i32(channels));
    fa_buffer_set_meta(buffer, fa_string("cropped"), fa_from_bool(cropped));
    fa_buffer_set_meta(buffer, fa_string("frames"), fa_i64(bufSize / (sizeof(double) * channels)));

    return buffer;
}
*/

fa_list_t fa_buffer_split(fa_buffer_t buffer, size_t size, bool copy)
{
    fa_list_t segments = fa_list_empty();
    size_t total_size = fa_buffer_size(buffer);
    uint8_t *buffer_ptr = fa_buffer_unsafe_address(buffer);
    for (size_t offset = 0; offset < total_size; offset += size) {
        size_t segment_size = size;
        if ((offset + size) > total_size) segment_size = total_size - offset;
        fa_buffer_t segment = NULL;
        if (copy) {
            uint8_t *data = fa_malloc(segment_size);
            memcpy(data, buffer_ptr + offset, segment_size);
            segment = fa_buffer_dwrap(data, segment_size);
        } else {
            segment = fa_buffer_wrap(buffer_ptr + offset, segment_size, NULL, NULL);
        }
        fa_push_list(segment, segments);
    }
    return fa_list_dreverse(segments);
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

static void buffer_take_reference(fa_ptr_t a)
{
    fa_buffer_take_reference(a);
}

static void buffer_release_reference(fa_ptr_t a)
{
    fa_buffer_release_reference(a);
}

fa_ptr_t buffer_get_meta(fa_ptr_t obj, fa_ptr_t key)
{
    return fa_buffer_get_meta(obj, key);
}

void buffer_set_meta(fa_ptr_t obj, fa_ptr_t key, fa_ptr_t value)
{
    fa_buffer_set_meta(obj, key, value);
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
    static fa_reference_count_t buffer_reference_count_impl = { buffer_take_reference, buffer_release_reference };
    static fa_meta_data_t buffer_meta_data_impl = { buffer_get_meta, buffer_set_meta };
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
        
    case fa_reference_count_i:
        return &buffer_reference_count_impl;
        
    case fa_meta_data_i:
        return &buffer_meta_data_impl;

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



