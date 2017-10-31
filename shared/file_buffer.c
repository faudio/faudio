
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2015
    All rights reserved.

 */

#include <fa/file_buffer.h>
#include <fa/error.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/atomic.h>
#include <fa/atomic/ring_buffer.h>
#include <fa/thread.h>

#include <sndfile.h>
#if _WIN32
SNDFILE* sf_wchar_open (const wchar_t *wpath, int mode, SF_INFO *sfinfo); // See note in io.c
#endif

#ifdef FA_MP3_IMPORT
#include <mpg123.h>
#endif

/*
    ## Notes
    
 */

#define kMaxPrintSize 80
#define file_buffer_warn(str) // do nothing

typedef sf_count_t (* fa_file_buffer_seek_function_t)(fa_file_buffer_t, size_t);
typedef void (* fa_file_buffer_cleanup_function_t)(fa_file_buffer_t);

#define kQueueSize 8

struct _fa_file_buffer_t {
    fa_impl_t         impl;

    size_t            file_size;
    FILE              *file;
    fa_string_t       path;

    size_t            total_buffer_size;
    size_t            single_buffer_size;
    size_t            offset1;
    size_t            offset2;
    uint8_t           *buffer1;
    uint8_t           *buffer2;
    uint8_t           *nextBuffer;
    
    size_t            file_pos;
    
    fa_file_buffer_seek_function_t seek_function;
    fa_file_buffer_cleanup_function_t cleanup_function;
    
    //fa_thread_t       thread;
    bool              done;
    fa_atomic_ring_buffer_t queue;

    fa_map_t          meta;
    
    fa_atomic_t       ref_count;
    fa_atomic_t       marked_for_destruction;
};


void file_buffer_fatal(char *msg, int error);


static fa_ptr_t file_buffer_thread_function(fa_ptr_t file_buffer);

static fa_file_buffer_t new_file_buffer(size_t size)
{
    assert(size > 1024 && "Too small buffer!");
    fa_ptr_t file_buffer_impl(fa_id_t interface);

    fa_file_buffer_t file_buffer = fa_new(file_buffer);

    file_buffer->impl = &file_buffer_impl;

    file_buffer->file      = 0;
    file_buffer->path      = NULL;
    file_buffer->file_pos  = 0;

    file_buffer->ref_count = fa_atomic();
    file_buffer->marked_for_destruction = fa_atomic();
    
    file_buffer->offset1   = -1;
    file_buffer->offset2   = -1;
    file_buffer->total_buffer_size = size;
    file_buffer->single_buffer_size = size / 2;
    file_buffer->buffer1   = fa_malloc(size);
    file_buffer->buffer2   = file_buffer->buffer1 + file_buffer->single_buffer_size;
    file_buffer->nextBuffer = file_buffer->buffer1;
    
    if (!file_buffer->buffer1) {
        if (errno == ENOMEM) {
            file_buffer_fatal("Out of memory", errno);
        } else {
            file_buffer_fatal("Unknown", errno);
        }
    }
    
    file_buffer->meta = fa_map_empty();
    fa_map_set_value_destructor(file_buffer->meta, fa_destroy);
    
    //fa_slog_info("new_file_buffer ", fa_i32(size));

    file_buffer->cleanup_function = NULL;
    file_buffer->seek_function = NULL;
    
    file_buffer->done   = false;
    file_buffer->queue  = fa_atomic_ring_buffer_create(kQueueSize * sizeof(long));

    //file_buffer_warn(fa_string("FileBuffer created"));
    return file_buffer;
}

static inline void do_destroy_file_buffer(fa_file_buffer_t file_buffer)
{
    fa_slog_info("do_destroy_file_buffer");
    // assert(fa_thread_current() == file_buffer->thread); Doesn't work! thread_current returns a new thread wrapper
    
    if (file_buffer->cleanup_function) {
        file_buffer->cleanup_function(file_buffer);
    }

    fa_free(file_buffer->buffer1); // Free buffer1, buffer2 is just a pointer halfways into buffer1

    fa_destroy(file_buffer->meta); // keys and values are automatically destroyed because the destructor is set

    fa_destroy(file_buffer->ref_count);
    fa_destroy(file_buffer->marked_for_destruction);
    fa_destroy(file_buffer->queue);
    if (file_buffer->path) fa_destroy(file_buffer->path);
    fa_delete(file_buffer);
    file_buffer_warn(fa_string("FileBuffer destroyed"));
}

static fa_ptr_t file_buffer_thread_function(fa_ptr_t file_buffer)
{
    fa_file_buffer_t fb = file_buffer;
    while (!fb->done) {
        fa_thread_sleep(50);
        while (fa_atomic_ring_buffer_can_read(fb->queue, sizeof(long)) && !fb->done) {
            long value;
            fa_atomic_ring_buffer_read_long(fb->queue, &value);
            size_t half_buffer_size = fb->single_buffer_size / 2;
            
            // Case 1: Comfortably inside current range
            if ((value >= fb->offset1 && value < (fb->offset1 + half_buffer_size)) ||
                (value >= fb->offset2 && value < (fb->offset2 + half_buffer_size)))
                continue;
            
            // Case 2: In second half of one buffer, but the other buffer comes directly after the first one
            if ((value >= fb->offset1 && value < (fb->offset1 + fb->single_buffer_size)
                && (fb->offset2 == fb->offset1 + fb->single_buffer_size)) ||
                (value >= fb->offset2 && value < (fb->offset2 + fb->single_buffer_size)
                && (fb->offset1 == fb->offset2 + fb->single_buffer_size))) {
                //printf("Yeah\n");
                continue;
            }
            
            // Otherwise: Seeking is needed
            
            //printf("Got value %ld which is outsize current range. Seeking into buffer %d\n",
            //    value, fb->nextBuffer == fb->buffer1 ? 1 : 2);
            
            // Move forward
            if (fb->nextBuffer == fb->buffer2 && value >= fb->offset1 && value < (fb->offset1 + fb->single_buffer_size)) {
                value = fb->offset1 + fb->single_buffer_size;
                //printf("Expanding to after 1\n");
            } else if (fb->nextBuffer == fb->buffer1 && value >= fb->offset2 && value < (fb->offset2 + fb->single_buffer_size)) {
                value = fb->offset2 + fb->single_buffer_size;
                //printf("Expanding to after 2\n");
            }
            
            fa_file_buffer_seek(fb, value);
            
            // Switch buffer
            fb->nextBuffer = fb->nextBuffer == fb->buffer1 ? fb->buffer2 : fb->buffer1;
        }
    }
    do_destroy_file_buffer(fb);
    return NULL;
}

static void default_buffer_close_(fa_file_buffer_t file_buffer)
{
    if (file_buffer->file) {
        FILE *file = file_buffer->file;
        file_buffer->file = NULL;
        fclose(file);
    }
}

static void audio_buffer_close_(fa_file_buffer_t file_buffer)
{
    if (file_buffer->file) {
        SNDFILE *sndfile = (SNDFILE*)(file_buffer->file);
        file_buffer->file = NULL;
        if (sf_close(sndfile)) {
            printf("Could not close sndfile\n");
        }
    }
}

#ifdef FA_MP3_IMPORT
static void mp3_buffer_close_(fa_file_buffer_t file_buffer) {
    if (file_buffer->file) {
        mpg123_handle *mp3 = (mpg123_handle*)(file_buffer->file);
        file_buffer->file = NULL;
        mpg123_close(mp3);
        mpg123_delete(mp3);
    }
}
#endif

void fa_file_buffer_destroy(fa_file_buffer_t file_buffer)
{
    fa_slog_info("fa_file_buffer_destroy");
    if ((size_t)fa_atomic_get(file_buffer->ref_count) == 0) {
        file_buffer->done = true;
    } else {
        fa_atomic_set(file_buffer->marked_for_destruction, (fa_ptr_t) true);
    }
}

void fa_file_buffer_take_reference(fa_file_buffer_t file_buffer)
{
    fa_atomic_add(file_buffer->ref_count, 1);
}

void fa_file_buffer_release_reference(fa_file_buffer_t file_buffer)
{
    fa_atomic_add(file_buffer->ref_count, -1);
    // TODO: is a lock needed?
    if ((size_t)fa_atomic_get(file_buffer->ref_count) == 0 && (bool)fa_atomic_get(file_buffer->marked_for_destruction)) {
        file_buffer->done = true;
    }
}

size_t fa_file_buffer_buffer_size(fa_file_buffer_t file_buffer)
{
    return file_buffer->total_buffer_size;
}

size_t fa_file_buffer_file_size(fa_file_buffer_t file_buffer)
{
    return file_buffer->file_size;
}

fa_string_t fa_file_buffer_path(fa_file_buffer_t file_buffer)
{
    return file_buffer->path;
}

fa_ptr_t fa_file_buffer_get_meta(fa_file_buffer_t file_buffer, fa_string_t name)
{
    return fa_map_dget(name, file_buffer->meta);
}

void fa_file_buffer_set_meta(fa_file_buffer_t file_buffer, fa_string_t name, fa_ptr_t value)
{
    file_buffer->meta = fa_map_dset(name, value, file_buffer->meta);
}

fa_map_t fa_file_buffer_meta(fa_file_buffer_t file_buffer)
{
    return file_buffer->meta;
}

static inline bool get_ptr(fa_file_buffer_t file_buffer, size_t index, void **ptr) {
    if (index >= file_buffer->file_size) {
        return false;
    }
    if (index >= file_buffer->offset1 && index < (file_buffer->offset1 + file_buffer->single_buffer_size)) {
        *ptr = file_buffer->buffer1 + index - file_buffer->offset1;
        return true;
    }
    if (index >= file_buffer->offset2 && index < (file_buffer->offset2 + file_buffer->single_buffer_size)) {
        *ptr = file_buffer->buffer2 + index - file_buffer->offset2;
        return true;
    }
    return false;
}

float fa_file_buffer_get_float(fa_file_buffer_t file_buffer, size_t index)
{
    void *ptr;
    if (get_ptr(file_buffer, index * sizeof(float), &ptr)) return *((float*) ptr);
    return 0;
}

double fa_file_buffer_get_double(fa_file_buffer_t file_buffer, size_t index)
{
    void *ptr;
    if (get_ptr(file_buffer, index * sizeof(double), &ptr)) return *((double*) ptr);
    return 0;
}

// void *fa_file_buffer_unsafe_address(fa_file_buffer_t file_buffer)
// {
//     return file_buffer->buffer;
// }

static sf_count_t default_seek_(fa_file_buffer_t file_buffer, size_t offset) {
    if (file_buffer->file_pos != offset) {
        if (fseek(file_buffer->file, offset, SEEK_SET) < 0) {
            return -1;
        }
    }
    
    if (file_buffer->nextBuffer == file_buffer->buffer1) {
        file_buffer->offset1 = offset;
    } else {
        file_buffer->offset2 = offset;
    }
        
    size_t bytes_read = fread(file_buffer->nextBuffer, 1, file_buffer->single_buffer_size, file_buffer->file);
    file_buffer->file_pos = offset + bytes_read;
    return file_buffer->file_pos;
}

static sf_count_t audio_seek_(fa_file_buffer_t file_buffer, size_t offset) {
    SNDFILE *sndfile = (SNDFILE*)file_buffer->file;
    
    //printf("=== audio_seek  file_pos: %zu   offset: %zu\n", file_buffer->file_pos, offset);
    
    fa_sample_type_t sample_type = fa_peek_integer(fa_get_meta(file_buffer, fa_string("sample-type")));
    int channels = fa_peek_integer(fa_get_meta(file_buffer, fa_string("channels")));
    uint8_t sample_size = fa_sample_type_size(sample_type);
    uint8_t frame_size = sample_size * channels;
    size_t frames = file_buffer->single_buffer_size / frame_size;
    
    size_t *nextOffset = file_buffer->nextBuffer == file_buffer->buffer1 ? &file_buffer->offset1 : &file_buffer->offset2;
    
    if (file_buffer->file_pos != offset) {
        sf_count_t pos = sf_seek(sndfile, offset / frame_size, SEEK_SET);
        //printf("fsf_seek(%lu) returned %lld\n", offset / frame_size, pos);
        if (pos < 0) {
            return pos;
        }
        *nextOffset = pos * channels * sample_size;
    } else {
        //printf("no need to seek, already at %zu\n", offset);
        *nextOffset = offset;
    }
    
    sf_count_t sz;
    switch (sample_type) {
        case float_sample_type:
            sz = sf_readf_float(sndfile, ((float *) file_buffer->nextBuffer), frames);
            break;
        case double_sample_type:
            sz = sf_readf_double(sndfile, ((double *) file_buffer->nextBuffer), frames);
            break;
    }
    file_buffer->file_pos = *nextOffset + (sz * frame_size);
    //printf("sz: %lld  %lu    --> %zu\n", sz, frames, file_buffer->file_pos);
    return file_buffer->file_pos;
}

#ifdef FA_MP3_IMPORT
static sf_count_t mp3_seek_(fa_file_buffer_t file_buffer, size_t offset) {
    mpg123_handle *mp3 = (mpg123_handle*)file_buffer->file;
    
    //printf("=== audio_seek  file_pos: %zu   offset: %zu\n", file_buffer->file_pos, offset);
    
    fa_sample_type_t sample_type = fa_peek_integer(fa_get_meta(file_buffer, fa_string("sample-type")));
    int channels = fa_peek_integer(fa_get_meta(file_buffer, fa_string("channels")));
    int encoding = fa_peek_int32(fa_get_meta(file_buffer, fa_string("native-encoding")));
    uint8_t sample_size = fa_sample_type_size(sample_type);
    uint8_t frame_size = sample_size * channels;
    
    size_t *nextOffset = file_buffer->nextBuffer == file_buffer->buffer1 ? &file_buffer->offset1 : &file_buffer->offset2;
    
    if (file_buffer->file_pos != offset) {
        off_t pos = mpg123_seek(mp3, offset / frame_size, SEEK_SET);
        //printf("mpg123_seek(%zu) returned %lld\n", offset / frame_size, pos);
        if (pos < 0) {
            return pos;
        }
        *nextOffset = pos * channels * sample_size;
    } else {
        //printf("no need to seek, already at %zu\n", offset);
        *nextOffset = offset;
    }
    
    int read_buffer_size = mpg123_outblock(mp3);
	unsigned char *read_buffer = fa_malloc(read_buffer_size);
    size_t pos = 0;
    bool done = false;
    
    do {
        size_t bytes_read;
        int error = mpg123_read(mp3, read_buffer, read_buffer_size, &bytes_read);
       
        if ((pos * sample_size + bytes_read) >= file_buffer->single_buffer_size) {
            bytes_read = file_buffer->single_buffer_size - (pos * sample_size);
            done = 1;
            if (bytes_read == 0) break;
        }
        
        size_t samples_read = bytes_read / sample_size;
        
        if (encoding == MPG123_ENC_FLOAT_32) {
            if (sample_type == float_sample_type) {
                memcpy(file_buffer->nextBuffer + (sample_size * pos), read_buffer, bytes_read);
            } else {
                for (int i = 0; i < samples_read; i++) {
                    file_buffer->nextBuffer[pos+i] = ((float*)read_buffer)[i];
                }
            }
        } else if (encoding == MPG123_ENC_FLOAT_64) {
            fa_slog_error("MPG123_ENC_FLOAT_64 not implemented");
            assert(false && "MPG123_ENC_FLOAT_64 not implemented");
            //memcpy(raw+pos, read_buffer, bytes_read);
        } else {
            assert(false && "Bad sample format (this should never happen)");
        }
        pos += samples_read;
        
        switch (error) {
            case MPG123_NEW_FORMAT: fa_warn(fa_format("new format, bytes_read: %zu", bytes_read)); break;
            case MPG123_DONE:       done = 1; break;
            case MPG123_OK:         break;
            default: {
                fa_free(read_buffer);
                return -1;
            }
        }
    } while (!done);
    fa_free(read_buffer);
    
    // We may have "overread", so check the file position with the library
    file_buffer->file_pos = mpg123_tell(mp3) * sample_size;

    return file_buffer->file_pos;
}
#endif

// --------------------------------------------------------------------------------

fa_file_buffer_t fa_file_buffer_create(fa_string_t path, size_t buffer_size)
{
    char *cpath = fa_string_to_utf8(path);
    FILE *file  = fa_fopen(cpath, "rb");
    fa_free(cpath);
    
    if (!file) {
        fa_string_t err = fa_string_dappend(fa_string("Could not read file "), fa_copy(path));
        return (fa_file_buffer_t) fa_error_create_simple(error, err, fa_string("Doremir.FileBuffer"));
    }
    
    // Get length of file
    fseek(file, 0, SEEK_END);
    long filelen = ftell(file);
    rewind(file);
    
    fa_file_buffer_t file_buffer = new_file_buffer(buffer_size);
    file_buffer->path = fa_copy(path);
    file_buffer->cleanup_function = default_buffer_close_;
    file_buffer->seek_function = default_seek_;
    file_buffer->file_size = filelen;
    file_buffer->file = (FILE*)file;
	
    fa_thread_t thread = fa_thread_create(file_buffer_thread_function, file_buffer, fa_string("File buffer (raw)"));
    fa_thread_detach(thread);
    
    return file_buffer;
}

fa_file_buffer_t fa_file_buffer_read_audio(fa_string_t path, size_t buffer_size, fa_sample_type_t sample_type)
{
    fa_file_buffer_t file_buffer;

    SNDFILE         *file;
    SF_INFO         info;
    info.format     = 0;

    {
        #if _WIN32
        wchar_t *cpath  = fa_string_to_utf16(path);
        file            = sf_wchar_open(cpath, SFM_READ, &info);
        #else
        char *cpath     = fa_string_to_utf8(path);
        file            = sf_open(cpath, SFM_READ, &info);
        #endif
        fa_free(cpath);

        if (sf_error(file)) {
            fa_string_t err = fa_string_dappend(fa_string("Could not read audio file "), fa_copy(path));
            return (fa_file_buffer_t) fa_error_create_simple(error, err, fa_string("Doremir.FileBuffer"));
        }
        
        fa_inform(fa_string_dappend(fa_string("Reading "), fa_copy(path)));
    }
    {
        uint8_t sample_size = fa_sample_type_size(sample_type);
        uint8_t frame_size = sample_size * info.channels;
        size_t data_size  = info.frames * frame_size;
        
        if (!info.seekable && (data_size > buffer_size)) {
            return (fa_file_buffer_t) fa_error_create_simple(error,
                fa_string("File not seekable"), fa_string("Doremir.FileBuffer"));
        }
        
        //printf("Requested buffer size: %zu\n", buffer_size);
        if (data_size < buffer_size) {
            // No need to create a bigger buffer than the actual file data
            buffer_size = data_size;
        } else {
            // Make sure buffer can contain an even number of frames
            size_t buffer_frames = buffer_size / frame_size;
            buffer_size = buffer_frames * frame_size;
        }
        //printf("Actual buffer size: %zu\n", buffer_size);
        file_buffer = new_file_buffer(buffer_size);
        file_buffer->path = fa_copy(path);
        file_buffer->cleanup_function = audio_buffer_close_;
        file_buffer->seek_function = audio_seek_;
        file_buffer->file_size = data_size;
        file_buffer->file = (FILE*)file;

        // Meta-data

        fa_file_buffer_set_meta(file_buffer, fa_string("frames"), fa_i64(info.frames));
        fa_file_buffer_set_meta(file_buffer, fa_string("sample-rate"), fa_i32(info.samplerate));
        fa_file_buffer_set_meta(file_buffer, fa_string("channels"), fa_i32(info.channels));
        fa_file_buffer_set_meta(file_buffer, fa_string("format"), fa_i32(info.format));
        fa_file_buffer_set_meta(file_buffer, fa_string("sample-size"), fa_i8(sample_size));
        fa_file_buffer_set_meta(file_buffer, fa_string("sample-type"), fa_i8(sample_type));

        fa_let(str, (char *) sf_get_string(file, SF_STR_TITLE))
        fa_file_buffer_set_meta(file_buffer, fa_string("title"), fa_string_from_utf8(str ? str : ""));

        fa_let(str, (char *) sf_get_string(file, SF_STR_SOFTWARE))
        fa_file_buffer_set_meta(file_buffer, fa_string("software"), fa_string_from_utf8(str ? str : ""));

        fa_let(str, (char *) sf_get_string(file, SF_STR_COPYRIGHT))
        fa_file_buffer_set_meta(file_buffer, fa_string("copyright"), fa_string_from_utf8(str ? str : ""));
        
        fa_file_buffer_set_meta(file_buffer, fa_string("audio-format"), fa_string("audio")); // TODO: set a more precise format
    }
	
    fa_thread_t thread = fa_thread_create(file_buffer_thread_function, file_buffer, fa_string("File buffer (audio)"));
    fa_thread_detach(thread);

    return file_buffer;
}

#ifdef FA_MP3_IMPORT
static fa_file_buffer_t mpg123_error_from_code(int code) {
    fa_string_t error_string = fa_format("mpg123 error: %s", mpg123_plain_strerror(code));
	fa_fail(fa_copy(error_string));
	return (fa_file_buffer_t) fa_error_create_simple(error, error_string, fa_string("Doremir.FileBuffer"));
}

static fa_file_buffer_t mpg123_error(mpg123_handle *handle) {
    fa_string_t error_string = fa_format("mpg123 error: %s", mpg123_strerror(handle));
    if (handle) mpg123_delete(handle);
	fa_fail(fa_copy(error_string));
	return (fa_file_buffer_t) fa_error_create_simple(error, error_string, fa_string("Doremir.FileBuffer"));
}

fa_file_buffer_t fa_file_buffer_read_mp3(fa_string_t path, size_t buffer_size, fa_sample_type_t sample_type)
{
    fa_file_buffer_t file_buffer = NULL;

    int error;
    mpg123_handle *mp3 = mpg123_new(NULL, &error);
	if (mp3 == NULL) return mpg123_error_from_code(error);
    mpg123_param(mp3, MPG123_ADD_FLAGS, MPG123_FORCE_FLOAT|MPG123_GAPLESS, 0);

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
	int channels = 0;
	int encoding = 0;
	long sample_rate = 0;
    char *cpath = fa_string_to_utf8(path);
    if (mpg123_open(mp3, cpath) || mpg123_getformat(mp3, &sample_rate, &channels, &encoding)) {
        fa_free(cpath);
        return mpg123_error(mp3);
    }
    fa_free(cpath);
    
    {
        mpg123_scan(mp3);
        size_t frames = mpg123_length(mp3);
        uint8_t sample_size = fa_sample_type_size(sample_type);
        uint8_t frame_size = sample_size * channels;
        size_t data_size  = frames * frame_size;
        
        //printf("Requested buffer size: %zu\n", buffer_size);
        if (data_size < buffer_size) {
            // No need to create a bigger buffer than the actual file data
            buffer_size = data_size;
        } else {
            // Make sure buffer can contain an even number of frames
            size_t buffer_frames = buffer_size / frame_size;
            buffer_size = buffer_frames * frame_size;
        }
        //printf("Actual buffer size: %zu\n", buffer_size);
        file_buffer = new_file_buffer(buffer_size);
        file_buffer->path = fa_copy(path);
        file_buffer->cleanup_function = mp3_buffer_close_;
        file_buffer->seek_function = mp3_seek_;
        file_buffer->file_size = data_size;
        file_buffer->file = (FILE*)mp3;

        // Meta-data

        fa_file_buffer_set_meta(file_buffer, fa_string("frames"), fa_i64(frames));
        fa_file_buffer_set_meta(file_buffer, fa_string("sample-rate"), fa_i32(sample_rate));
        fa_file_buffer_set_meta(file_buffer, fa_string("channels"), fa_i32(channels));
        fa_file_buffer_set_meta(file_buffer, fa_string("format"), fa_string("mp3"));
        fa_file_buffer_set_meta(file_buffer, fa_string("sample-size"), fa_i8(sample_size));
        fa_file_buffer_set_meta(file_buffer, fa_string("sample-type"), fa_i8(sample_type));
        fa_file_buffer_set_meta(file_buffer, fa_string("native-encoding"), fa_i32(encoding));
        
        fa_file_buffer_set_meta(file_buffer, fa_string("audio-format"), fa_string("mp3"));
    }
	
    fa_thread_t thread = fa_thread_create(file_buffer_thread_function, file_buffer, fa_string("File buffer (mp3)"));
    fa_thread_detach(thread);

    return file_buffer;
}
#endif

size_t fa_file_buffer_seek(fa_file_buffer_t file_buffer, size_t pos)
{
    fa_file_buffer_seek_function_t seek_function = file_buffer->seek_function;
    return seek_function(file_buffer, pos);
}

size_t fa_file_buffer_seek_if_needed(fa_file_buffer_t file_buffer, size_t pos)
{
    if ((pos < file_buffer->offset1 || pos >= (file_buffer->offset1 + file_buffer->single_buffer_size)) &&
        (pos < file_buffer->offset2 || pos >= (file_buffer->offset2 + file_buffer->single_buffer_size))) {
        return fa_file_buffer_seek(file_buffer, pos);
    } else {
        return -2;
    }
}

void fa_file_buffer_hint(fa_file_buffer_t file_buffer, size_t offset)
{
    fa_atomic_ring_buffer_write_long(file_buffer->queue, offset);
}


// --------------------------------------------------------------------------------

void file_buffer_destroy(fa_ptr_t a)
{
    fa_file_buffer_destroy(a);
}

void file_buffer_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    if (p(a)) fa_file_buffer_destroy(a);
}

fa_string_t file_buffer_show(fa_ptr_t a)
{
    fa_file_buffer_t file_buffer = (fa_file_buffer_t) a;
    fa_string_t str = fa_string("<FileBuffer ");
    if (file_buffer->path) {
        str = fa_string_dappend(str, fa_copy(file_buffer->path));
    } else {
        str = fa_string_dappend(str, fa_string("(no file)"));
    }
    str = fa_string_dappend(str, fa_string(">"));
    return str;
}

static void file_buffer_take_reference(fa_ptr_t a)
{
    fa_file_buffer_take_reference(a);
}

static void file_buffer_release_reference(fa_ptr_t a)
{
    fa_file_buffer_release_reference(a);
}

static fa_ptr_t file_buffer_get_meta(fa_ptr_t obj, fa_ptr_t key)
{
    return fa_file_buffer_get_meta(obj, key);
}

static void file_buffer_set_meta(fa_ptr_t obj, fa_ptr_t key, fa_ptr_t value)
{
    return fa_file_buffer_set_meta(obj, key, value);
}

static fa_map_t file_buffer_meta_map(fa_ptr_t obj)
{
    return fa_file_buffer_meta(obj);
}

fa_dynamic_type_repr_t file_buffer_get_type(fa_ptr_t a)
{
    return file_buffer_type_repr;
}

fa_ptr_t file_buffer_impl(fa_id_t interface)
{
    static fa_string_show_t file_buffer_show_impl = { file_buffer_show };
    static fa_destroy_t file_buffer_destroy_impl = { file_buffer_destroy, file_buffer_deep_destroy };
    static fa_reference_count_t file_buffer_reference_count_impl = { file_buffer_take_reference, file_buffer_release_reference };
    static fa_meta_data_t file_buffer_meta_data_impl = { file_buffer_get_meta, file_buffer_set_meta, file_buffer_meta_map };
    static fa_dynamic_t file_buffer_dynamic_impl = { file_buffer_get_type };

    switch (interface) {
    case fa_destroy_i:
        return &file_buffer_destroy_impl;

    case fa_string_show_i:
        return &file_buffer_show_impl;
        
    case fa_dynamic_i:
        return &file_buffer_dynamic_impl;
        
    case fa_reference_count_i:
        return &file_buffer_reference_count_impl;
    
    case fa_meta_data_i:
        return &file_buffer_meta_data_impl;
        
    default:
        return NULL;
    }
}


void file_buffer_fatal(char *msg, int error)
{
    void fa_log_error_from(fa_string_t msg, fa_string_t origin);

    fa_log_error_from(fa_string_dappend(fa_string_from_utf8(msg), fa_format_integral(" (error code %d)", error)), fa_string("Doremir.FileBuffer"));
    fa_log_error(fa_string("Terminating Audio Engine"));
    exit(error);
}



