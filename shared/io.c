
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/fa.h>
#include <fa/io.h>
#include <fa/util.h>

#include <sndfile.h>

struct filter_base {
    fa_impl_t impl;
    fa_ptr_t data1, data2, data3, data4;
};

#define byte_t uint8_t

// #define io_printf printf
#define io_printf(fmt, ...) // do nothing

fa_io_filter_t fa_io_ref_filter(fa_ptr_t r);

void fa_io_pull(fa_io_source_t source,
                fa_io_callback_t bufferCallback,
                fa_ptr_t ptr)
{
    fa_io_pull_through((fa_io_filter_t) source, NULL, bufferCallback, ptr);
}


void fa_io_push(fa_io_sink_t sink, fa_buffer_t buffer)
{
    fa_io_push_through((fa_io_filter_t) sink, NULL, buffer);
}

void fa_io_pull_through(fa_io_filter_t filter,
                        fa_io_source_t upstream,
                        fa_io_callback_t bufferCallback,
                        fa_ptr_t ptr)
{
    ((fa_io_filter_interface_t *) fa_interface(fa_io_filter_interface_i, filter))->pull(filter, upstream, bufferCallback, ptr);
}


void fa_io_push_through(fa_io_filter_t filter, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    ((fa_io_filter_interface_t *) fa_interface(fa_io_filter_interface_i, filter))->push(filter, downstream, buffer);
}



#define FILTER_IMPLEMENTATION(T) \
    fa_ptr_t T##_impl(fa_id_t interface)                            \
    {                                                               \
        static fa_destroy_t T##_destroy_impl = { T##_destroy };     \
        static fa_string_show_t T##_show_impl = { T##_show };       \
        static fa_io_filter_interface_t T##_io_filter_impl          \
            = { T##_pull, T##_push };                               \
                                                                    \
        switch (interface) {                                        \
        case fa_destroy_i: return &T##_destroy_impl;                \
        case fa_string_show_i: return &T##_show_impl;               \
        case fa_io_filter_interface_i: return &T##_io_filter_impl;  \
        default: return NULL;                                       \
        }                                                           \
    }


// ------------------------------------------------------------------------------------------

void split_filter_destroy(fa_ptr_t x)
{
    fa_warn(fa_string("Unimplemented IO destroy"));
}

fa_string_t split_filter_show(fa_ptr_t x)
{
    return fa_string("<SplitFilter>");
}

static inline void _split_pull(fa_ptr_t x, fa_buffer_t buffer)
{
    fa_unpair(x, sink2, closure) {
        if (buffer) {
            fa_io_push(sink2, buffer);
            fa_unpair(closure, callback, data) {
                ((fa_io_callback_t) callback)(data, buffer);
            }
        } else {
            fa_io_push(sink2, NULL);
            fa_unpair(closure, callback, data) {
                ((fa_io_callback_t) callback)(data, NULL);
            }
        }
    }
}
void split_filter_pull(fa_ptr_t x, fa_io_source_t source, fa_io_callback_t callback, fa_ptr_t data)
{
    fa_io_sink_t sink2 = ((struct filter_base *) x)->data1;
    fa_io_pull(source, _split_pull, fa_pair_create(sink2, fa_pair_create(callback, data)));
}
void split_filter_push(fa_ptr_t x, fa_io_sink_t sink, fa_buffer_t buffer)
{
    fa_io_sink_t sink2 = ((struct filter_base *) x)->data1;
    fa_io_push(sink2, buffer);
    fa_io_push(sink, buffer);
}
FILTER_IMPLEMENTATION(split_filter);


// ------------------------------------------------------------------------------------------

void stdin_filter_destroy(fa_ptr_t x)
{
    fa_warn(fa_string("Unimplemented IO destroy"));
}

fa_string_t stdin_filter_show(fa_ptr_t x)
{
    return fa_string("<StdInSource>");
}

inline static
char *read_line(char *in)
{
    char *cptr;

    if ((cptr = fgets(in, 80, stdin))) {
        while (*cptr == ' ' || *cptr == '\t') {
            cptr++;
        }

        return cptr;
    } else {
        return 0;
    }
}

void stdin_filter_pull(fa_ptr_t _, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
    char in[80]; // TODO max length to read_line

    read_line(in);
    callback(data, fa_buffer_wrap(in, strlen(in), NULL, NULL)); // Wrap stack var, no dealloc
}
void stdin_filter_push(fa_ptr_t _, fa_io_sink_t downstream, fa_buffer_t buffer)
{
}
FILTER_IMPLEMENTATION(stdin_filter);


// ------------------------------------------------------------------------------------------

void standardout_filter_destroy(fa_ptr_t x)
{
    fa_warn(fa_string("Unimplemented IO destroy"));
}

fa_string_t standardout_filter_show(fa_ptr_t x)
{
    return fa_string("<StdOutSink>");
}

void standardout_filter_pull(fa_ptr_t _, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
}
void standardout_filter_push(fa_ptr_t _, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    if (buffer) {
        for (int i = 0; i < fa_buffer_size(buffer); ++i) {
            int c = fa_buffer_get(buffer, i);
            putchar(c);
        }
    } else {
        // Can not close standard out
    }
}
FILTER_IMPLEMENTATION(standardout_filter);


// ------------------------------------------------------------------------------------------

void write_filter_destroy(fa_ptr_t x)
{
    fa_warn(fa_string("Unimplemented IO destroy"));
}

fa_string_t write_filter_show(fa_ptr_t x)
{
    return fa_string("<WriteSink>");
}

void write_filter_pull(fa_ptr_t _, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
}
void write_filter_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    // inform(fa_string("In write_filter push"));

    if (buffer) {
        fa_string_t path = /*fa_copy*/(((struct filter_base *) x)->data1);
        char* _path = fa_unstring(path);
        FILE *fp = fopen(_path, "ab");
        fa_free(_path);

        if (!fp) {
            fa_fail(fa_string_dappend(fa_string("Could not write file: "), fa_copy(path)));
            fa_fail(fa_string_format_integral("  errno: %d", errno));
            fclose(fp);
        } else {
            fwrite(fa_buffer_unsafe_address(buffer), fa_buffer_size(buffer), 1, fp);
            fclose(fp);
        }
    } else {
        // TODO close
    }
}
FILTER_IMPLEMENTATION(write_filter);


// ------------------------------------------------------------------------------------------

void read_filter_destroy(fa_ptr_t x)
{
    fa_inform(fa_string("In read_filter_destroy"));
    struct filter_base *filter = ((struct filter_base *) x);
    if (filter->data1) fa_destroy(filter->data1);
    if (filter->data2) fa_destroy(filter->data2);
    if (filter->data3) fa_destroy(filter->data3);
    fa_free(filter);
}

fa_string_t read_filter_show(fa_ptr_t x)
{
    return fa_string("<ReadSource>");
}

void read_filter_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
    // inform(fa_string("In read_filter push"));
    struct filter_base *filter = ((struct filter_base *) x);
    fa_string_t path = fa_copy(filter->data1);
    fa_ptr_t start = filter->data2;
    fa_ptr_t end   = filter->data3;
    size_t startBytes = start ? fa_peek_integer(start) : 0;
    size_t endBytes   = end   ? fa_peek_integer(end)   : 0;
    char *cpath = fa_unstring(path);
    FILE *fp = fopen(cpath, "rb");
    fa_free(cpath);

    if (!fp) {
        fa_fail(fa_string_dappend(fa_string("Could not read file: "), path));
    } else {
        byte_t raw[1024 * 8];
        size_t buffer_bytes = sizeof(raw);
        size_t bytes_left = endBytes - startBytes; // invalid but ignored if end is NULL
        
        fseek(fp, startBytes, SEEK_SET);
        while (!ferror(fp) && !feof(fp) && (!end || bytes_left > 0)) {
            size_t bytes_to_read = (end && (bytes_left < buffer_bytes)) ? bytes_left : buffer_bytes;
            size_t bytes_read = fread(raw, 1, bytes_to_read, fp);
            fa_buffer_t buffer = fa_buffer_wrap(raw, bytes_read, NULL, NULL);
            callback(data, buffer);
            fa_destroy(buffer);
            bytes_left -= bytes_read;
        }
        // Wrapped stack, no dealloc
    }

    callback(data, NULL);
    
    fa_destroy(path);
    
    fclose(fp);
}

void read_filter_push(fa_ptr_t _, fa_io_sink_t downstream, fa_buffer_t buffer)
{
}
FILTER_IMPLEMENTATION(read_filter);


// ------------------------------------------------------------------------------------------

void read_audio_filter_destroy(fa_ptr_t x)
{
    fa_inform(fa_string("In read_audio_filter_destroy"));
    struct filter_base *filter = ((struct filter_base *) x);
    if (filter->data1) fa_destroy(filter->data1);
    if (filter->data2) fa_destroy(filter->data2);
    if (filter->data3) fa_destroy(filter->data3);
    fa_free(filter);
}

fa_string_t read_audio_filter_show(fa_ptr_t x)
{
    return fa_string("<ReadAudioSource>");
}

void read_audio_filter_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
    // inform(fa_string("In read_filter push"));

    struct filter_base *filter = ((struct filter_base *) x);
    fa_string_t path    = fa_copy(filter->data1);
    fa_ptr_t start      = filter->data2;
    fa_ptr_t end        = filter->data3;
    size_t start_frames = start ? fa_peek_integer(start) : 0;
    
    char *cpath = fa_unstring(path);
    SF_INFO info;
    info.format = 0;
    SNDFILE *sndfile = sf_open(cpath, SFM_READ, &info);
    fa_free(cpath);

    if (sf_error(sndfile)) {
        fa_fail(fa_string_dappend(fa_string("Could not read audio file: "), path));
    } else if (start_frames && !info.seekable) {
        fa_fail(fa_string_dappend(fa_string("Audio file not seekable: "), path));
    } else if (start_frames > info.frames) {
        fa_fail(fa_string_dappend(fa_string("Start is beyond end of file: "), path));
    } else {
        size_t end_frames = end ? fa_peek_integer(end) : info.frames;
        if (end_frames > info.frames) end_frames = info.frames;
        
        fa_sample_type_t sample_type = double_sample_type; // TODO: don't hardcode sample_type
        uint8_t sample_size = fa_sample_type_size(sample_type);
        uint8_t frame_size = sample_size * info.channels;
        if (start_frames) {
            sf_seek(sndfile, start_frames, SEEK_SET);
        }
        
        size_t buffer_frames = 1024;
        size_t buffer_size = buffer_frames * frame_size;
        byte_t *raw = fa_malloc(buffer_size);
        size_t frames_left = end_frames - start_frames;
        
        while (!sf_error(sndfile) && frames_left > 0) {
            size_t frames_to_read = (frames_left < buffer_frames) ? frames_left : buffer_frames;
            size_t frames_read = sf_readf_double(sndfile, ((double *) raw), frames_to_read);
            fa_buffer_t buffer = fa_buffer_wrap(raw, frames_read * frame_size, NULL, NULL);
            callback(data, buffer);
            fa_destroy(buffer);
            frames_left -= frames_read;
        }
        
        fa_free(raw);
    }

    callback(data, NULL);
    
    fa_destroy(path);
    
    sf_close(sndfile);
}

void read_audio_filter_push(fa_ptr_t _, fa_io_sink_t downstream, fa_buffer_t buffer)
{
}
FILTER_IMPLEMENTATION(read_audio_filter);


// ------------------------------------------------------------------------------------------

void ref_filter_destroy(fa_ptr_t x)
{
    fa_warn(fa_string("Unimplemented IO destroy"));
}

fa_string_t ref_filter_show(fa_ptr_t x)
{
    return fa_string("<Ref>");
}

void ref_filter_pull(fa_ptr_t x, fa_io_source_t _, fa_io_callback_t callback, fa_ptr_t data)
{
    fa_buffer_t *r = ((struct filter_base *) x)->data1;
    callback(data, *r);
}
void ref_filter_push(fa_ptr_t x, fa_io_sink_t _, fa_buffer_t buffer)
{
    fa_buffer_t *r = ((struct filter_base *) x)->data1;
    *r = buffer;
}
FILTER_IMPLEMENTATION(ref_filter);


// ------------------------------------------------------------------------------------------

void identity_destroy(fa_ptr_t x)
{
    fa_warn(fa_string("Unimplemented IO destroy"));
}

fa_string_t identity_show(fa_ptr_t x)
{
    return fa_string("<Id>");
}

void identity_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
    fa_io_pull(upstream, callback, data);
}
void identity_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    fa_io_push(downstream, buffer);
}
FILTER_IMPLEMENTATION(identity);



// ------------------------------------------------------------------------------------------

void composed_filter_destroy(fa_ptr_t x)
{
    fa_warn(fa_string("Unimplemented IO destroy"));
}

fa_string_t  composed_filter_show(fa_ptr_t x)
{
    return fa_string("<Composed>");
}

static inline void _composed_pull2(fa_ptr_t x, fa_buffer_t buffer)
{
    fa_unpair(x, callback, data) {
        ((fa_io_callback_t) callback)(data, buffer);
    }
}
static inline void _composed_pull(fa_ptr_t x, fa_buffer_t buffer)
{
    fa_unpair(x, f2, closure) {
        fa_io_pull_through(f2, (fa_io_source_t) fa_io_ref_filter(&buffer), _composed_pull2, closure);
    }
}

void composed_filter_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
    fa_io_filter_t f1 = ((struct filter_base *) x)->data1;
    fa_io_filter_t f2 = ((struct filter_base *) x)->data2;

    fa_with_temp(closure, fa_pair_create(callback, data)) {
        fa_with_temp(x, fa_pair_create(f2, closure)) {
            fa_io_pull_through(f1, upstream, _composed_pull, x);
        }
    }
}
void composed_filter_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    fa_io_filter_t f1 = ((struct filter_base *) x)->data1;
    fa_io_filter_t f2 = ((struct filter_base *) x)->data2;

    fa_buffer_t buffer2;
//     fa_io_push_through((fa_io_sink_t) fa_io_ref_filter(&buffer2), f1, buffer);
//     fa_slog_info("buffer2 is now ", buffer2);
//     fa_io_push_through(f2, downstream, buffer2);
    
    fa_io_push_through(f1, (fa_io_sink_t) fa_io_ref_filter(&buffer2), buffer);
    fa_io_push_through(f2, downstream, buffer2);
}
FILTER_IMPLEMENTATION(composed_filter);


// ------------------------------------------------------------------------------------------

void simple_filter_destroy(fa_ptr_t x)
{
    fa_inform(fa_string("simple filter destroy"));
    fa_nullary_t destructor = ((struct filter_base *) x)->data4;
    if (destructor) {
        destructor(x);
    }
    // fa_free(x); // ?
}

fa_string_t simple_filter_show(fa_ptr_t x)
{
    return fa_string("<Simple>");
}

void _simple_filter_pull1(fa_ptr_t y, fa_buffer_t buffer)
{
    fa_unpair(y, x, closure) {
        fa_io_callback_t      simple_push = ((struct filter_base *) x)->data1;
        fa_io_read_callback_t simple_pull = ((struct filter_base *) x)->data2;
        fa_ptr_t                   cbData = ((struct filter_base *) x)->data3;

        fa_unpair(closure, callback, data) {
            simple_push(cbData, buffer);
            simple_pull(cbData, callback, data);
        }
    }
}

// pull from upstream, push to x, pull from x
void simple_filter_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, fa_ptr_t data)
{
    if (upstream) {
        fa_io_pull(upstream, _simple_filter_pull1, fa_pair_create(x, fa_pair_create(callback, data)));
    } else {
        fa_io_read_callback_t simple_pull = ((struct filter_base *) x)->data2;
        fa_ptr_t                   cbData = ((struct filter_base *) x)->data3;
        simple_pull(cbData, callback, data);
    }
}

void _simple_push1(fa_ptr_t downstream, fa_buffer_t buffer)
{
    fa_io_push(downstream, buffer);
}

// push to x, pull from x, push downstream
void simple_filter_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    fa_io_callback_t      simple_push = ((struct filter_base *) x)->data1;
    fa_io_read_callback_t simple_pull = ((struct filter_base *) x)->data2;
    fa_ptr_t                   cbData = ((struct filter_base *) x)->data3;

    if (simple_push) simple_push(cbData, buffer);
    if (simple_pull) simple_pull(cbData, _simple_push1, downstream);
}
FILTER_IMPLEMENTATION(simple_filter);


// ------------------------------------------------------------------------------------------

fa_io_filter_t fa_io_split(fa_io_sink_t sink)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &split_filter_impl;
    x->data1 = sink;
    return (fa_io_filter_t) x;
}

fa_io_filter_t fa_io_compose(fa_io_filter_t f1, fa_io_filter_t f2)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &composed_filter_impl;
    x->data1 = f1;
    x->data2 = f2;
    return (fa_io_filter_t) x;
}

fa_io_source_t fa_io_apply(fa_io_source_t f1, fa_io_filter_t f2)
{
    return (fa_io_source_t) fa_io_compose((fa_io_filter_t) f1, f2);
}

fa_io_sink_t fa_io_coapply(fa_io_filter_t f1, fa_io_sink_t f2)
{
    return (fa_io_sink_t) fa_io_compose(f1, (fa_io_filter_t) f2);
}


fa_io_filter_t fa_io_ref_filter(fa_ptr_t r)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &ref_filter_impl;
    x->data1 = r;
    return (fa_io_filter_t) x;
}

fa_io_filter_t fa_io_identity()
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &identity_impl;
    return (fa_io_filter_t) x;
}

fa_io_source_t fa_io_standard_in()
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &stdin_filter_impl;
    return (fa_io_source_t) x;
}

fa_io_sink_t fa_io_standard_out()
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &standardout_filter_impl;
    return (fa_io_sink_t) x;
}

fa_io_sink_t fa_io_write_file(fa_string_t path)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &write_filter_impl;
    x->data1 = fa_copy(path);
    return (fa_io_sink_t) x;
}

fa_io_source_t fa_io_read_file_between(fa_string_t path, fa_ptr_t start, fa_ptr_t end)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &read_filter_impl;
    x->data1 = path;
    x->data2 = start;
    x->data3 = end;
    return (fa_io_source_t) x;
}

fa_io_source_t fa_io_read_file(fa_string_t path)
{
    return fa_io_read_file_between(path, NULL, NULL);
}

fa_io_source_t fa_io_read_audio_file_between(fa_string_t path, fa_ptr_t startFrames, fa_ptr_t endFrames)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &read_audio_filter_impl;
    x->data1 = path;
    x->data2 = startFrames;
    x->data3 = endFrames;
    return (fa_io_source_t) x;
}

fa_io_source_t fa_io_read_audio_file(fa_string_t path)
{
    return fa_io_read_audio_file_between(path, NULL, NULL);
}


fa_io_filter_t fa_io_create_simple_filter(fa_io_callback_t callback,
                                          fa_io_read_callback_t readCallback,
                                          fa_ptr_t data)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &simple_filter_impl;
    x->data1 = callback;
    x->data2 = readCallback;
    x->data3 = data;
    x->data4 = NULL;
    return (fa_io_filter_t) x;
}

fa_io_filter_t fa_io_create_simple_filter_with_destructor(fa_io_callback_t callback,
                                                          fa_io_read_callback_t readCallback,
                                                          fa_ptr_t data,
                                                          fa_nullary_t destructor)
{
    struct filter_base *x = (struct filter_base *)fa_io_create_simple_filter(callback, readCallback, data);
    x->data4 = destructor;
    return (fa_io_filter_t) x;
}


void push_ringbuffer(fa_ptr_t x, fa_buffer_t buffer)
{
    assert(false && "Not implemented");
}

// DEBUG
size_t fa_atomic_ring_buffer_remaining(fa_atomic_ring_buffer_t buffer);
size_t bytes_read = 0;



void pull_ringbuffer(fa_ptr_t x, fa_io_callback_t cb, fa_ptr_t data)
{
    fa_atomic_ring_buffer_t rbuffer = x;
    // fa_mark_used(rbuffer);
    
    while (!fa_atomic_ring_buffer_is_closed(rbuffer)) {
        size_t size = 256;

        // io_printf(">>>> Remaining: %zu\n", fa_atomic_ring_buffer_remaining(rbuffer));
        if (fa_atomic_ring_buffer_can_read(rbuffer, size)) {
            io_printf(">>>>>>>>>> Reading size: %zu\n", size);

            fa_buffer_t buf = fa_buffer_create(size);
            byte_t *raw = fa_buffer_unsafe_address(buf);
            bytes_read += size;

            for (size_t i = 0; i < size; ++i) {
                bool success = fa_atomic_ring_buffer_read(rbuffer, raw + i);
                assert(success && "Could not read from ring buffer");
            }

            cb(data, buf);
            fa_destroy(buf);
        } else {
            fa_thread_sleep(1);
        }
    }

    // Buffer closed
    assert(fa_atomic_ring_buffer_is_closed(rbuffer));
    {
        // fa_warn(fa_string_format_integral("Bytes read: %zu", bytes_read));

// should be 1
// we drop last frame as ogg converter (and others) expect an even number of samples
#define kMaxDrainSpill 8

        for (size_t size = 256; size >= kMaxDrainSpill; size /= 2) {
            while (fa_atomic_ring_buffer_can_read(rbuffer, size)) {
                io_printf(">>>>>>>>>> End reading size: %zu\n", size);

                fa_buffer_t buf = fa_buffer_create(size);
                uint8_t *raw = fa_buffer_unsafe_address(buf);
                bytes_read += size;

                for (size_t i = 0; i < size; ++i) {
                    bool success = fa_atomic_ring_buffer_read(rbuffer, raw + i);
                    assert(success && "Could not read from ring buffer");
                }

                cb(data, buf);
                fa_destroy(buf);
            }
        }

        // Nothing more to read, close downstream and finish
        cb(data, NULL);
        assert(fa_atomic_ring_buffer_remaining(rbuffer) < kMaxDrainSpill);
    }

}

fa_io_source_t fa_io_from_ring_buffer(fa_atomic_ring_buffer_t rbuffer)
{
    return (fa_io_source_t) fa_io_create_simple_filter(push_ringbuffer, pull_ringbuffer, rbuffer);
}



void pull_buffer(fa_ptr_t x, fa_io_callback_t cb, fa_ptr_t data)
{
    size_t chunk_size = 256;
    fa_buffer_t buffer = x;
    size_t size = fa_buffer_size(buffer);
    size_t chunks = size / chunk_size;
    byte_t *raw_source = fa_buffer_unsafe_address(buffer);
    
    for (int chunk = 0; chunk < chunks; chunk++) {
        fa_buffer_t buf = fa_buffer_create(chunk_size);
        byte_t *raw_dest = fa_buffer_unsafe_address(buf);
        memcpy(raw_dest, raw_source + (chunk * chunk_size), chunk_size);
        cb(data, buf);
        fa_destroy(buf);
    }
    
    size_t remaining_bytes = size - (chunk_size * chunks);
    if (remaining_bytes > kMaxDrainSpill) {
        remaining_bytes = kMaxDrainSpill * (remaining_bytes / kMaxDrainSpill);
        fa_buffer_t buf = fa_buffer_create(remaining_bytes);
        byte_t *raw_dest = fa_buffer_unsafe_address(buf);
        memcpy(raw_dest, raw_source + (chunks * chunk_size), remaining_bytes);
        cb(data, buf);
        fa_destroy(buf);
    }

    // Nothing more to read, close downstream and finish
    cb(data, NULL);
}

fa_io_source_t fa_io_from_buffer(fa_buffer_t buffer) {
    return (fa_io_source_t) fa_io_create_simple_filter(NULL, pull_buffer, buffer);
}



static inline void _run(fa_ptr_t pair, fa_buffer_t buffer)
{
    fa_unpair(pair, sink, ok) {
        if (!buffer) {
            *((bool *) ok) = false;
        } else {
            fa_io_push(sink, buffer);
        }
    }
}
void fa_io_run(fa_io_source_t source, fa_io_sink_t sink)
{
    bool ok = true;
    fa_with_temp(pair, fa_pair_create(sink, &ok)) {
        while (ok) {
            fa_io_pull(source, _run, pair);
        }
    }
}

struct _pull_to_buffer_info {
    void *ptr;
    bool ok;
    size_t allocated;
    size_t used;
    size_t growth;
};

// static fa_ptr_t default_destroy(fa_ptr_t _, fa_ptr_t data)
// {
//     fa_free(data);
//     return NULL;
// }

static void _pull_to_buffer(fa_ptr_t i, fa_buffer_t buf)
{
    // buf is the current small buffer
    struct _pull_to_buffer_info *info = i;
    if (!buf) {
        info->ok = false;
    } else {
        size_t add = fa_buffer_size(buf);
        
        // Grow if needed
        if ((info->used + add) > info->allocated) {
            size_t new_allocate_size = info->allocated + info->growth;
            if (new_allocate_size < info->allocated + add) {
                new_allocate_size = info->allocated + add;
                info->growth = add;
            }
            //printf("Allocating more memory for buffer, new size: %zu\n", new_allocate_size);
            info->ptr = fa_realloc(info->ptr, new_allocate_size);
            if (info->ptr) {
                info->allocated = new_allocate_size;
            } else {
                info->allocated = 0;
                info->used = 0;
                info->ok = false;
                fa_fail(fa_string("fa_io_pull_to_buffer: could not allocate memory"));
                return;
            }
        }
        
        // Copy data and increase used
        memcpy(info->ptr + info->used, fa_buffer_unsafe_address(buf), add);
        info->used += add;
    }
}

fa_buffer_t fa_io_pull_to_buffer(fa_io_source_t source)
{
    size_t start_size = 65536;
    struct _pull_to_buffer_info info = { fa_malloc(start_size), true, start_size, 0, start_size };
    while (info.ok) {
        fa_io_pull(source, _pull_to_buffer, (fa_ptr_t)&info);
    }
    if (info.ptr && info.used) {
        return fa_buffer_dwrap(info.ptr, info.used);
    } else {
        if (info.ptr) {
            fa_free(info.ptr);
        }
        return NULL;
    }
}

