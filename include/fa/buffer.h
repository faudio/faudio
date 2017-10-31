
#ifndef _FA_BUFFER
#define _FA_BUFFER

#include <fa.h>
#include <fa/std.h>
#include <fa/pair.h>
#include <fa/string.h>
#include <fa/map.h>
#include <config.h>

/** @addtogroup FaBuffer
 
    Mutable buffers. 
    
    Each buffer is a mutable sequence of bytes with single-ownership semantics. The length
    of a buffer can never change, but its memory can be reused using @ref fa_buffer_dresize.
    
    The contents of a buffer is *untyped*, i.e. it is possible to write 16-bit integer to
    a buffer and subsequently read floats. The user must assure that the correct type is
    being used. All `get` and `set` methods are *fail-fast*: reading or writing past the
    size results in assertion errors in a debug build and undefined behaviour in a release
    build.
    
    @par Literals
    - `buffer(size)` 

    @par Implements 
    - fa_copy_t
    - fa_destroy_t
    - fa_string_show_t
    - fa_reference_count
    - fa_meta_data

    @see 
    - [Data structures](@ref DataStructures)
 
    @defgroup Fa Fa
    @{
    @defgroup FaBuffer Buffer
    @{
    */

/** @typedef fa_buffer_t
    The buffer. 
*/
typedef struct _fa_buffer_t * fa_buffer_t;

/** Create a new buffer and initialize it to zero-bytes.
    @note
        O(n)
*/
fa_buffer_t fa_buffer_create(size_t size_);

/** Create a buffer wrapping the given memory region.

    @param ptr  Pointer to wrap.
    @param size Number of bytes to wrap.
    @param destroy_function Function to call upon destruction (optional).
    @param destroy_data Data closed over by the destroy function.
    @note
        O(1)
*/
fa_buffer_t fa_buffer_wrap(fa_ptr_t ptr,
                           size_t size_,
                           fa_unary_t unary,
                           fa_ptr_t ptr_);
                           
/** Create a buffer wrapping the given memory region.
    The memory is released (using fa_free) when the
    buffer is destroyed.

    @param ptr  Pointer to wrap.
    @param size Number of bytes to wrap.
    @note
        O(1)
*/
fa_buffer_t fa_buffer_dwrap(fa_ptr_t ptr, size_t size_);

/** Copy the given buffer.
    @note
        O(n)
*/
fa_buffer_t fa_buffer_copy(fa_buffer_t buffer);

/** Return a new buffer of the given size containing the contents
    of the given buffer padded with zero bytes at the end.
    @note
        O(n)
*/
fa_buffer_t fa_buffer_resize(size_t size_, fa_buffer_t buffer);

/** Return a new buffer of the given size containing the contents
    of the given buffer padded with zero bytes at the end.
    
    The given buffer is destroyed. This function can reuse memory
    from the destroyed buffer and be considerably faster than @ref fa_buffer_resize.
    
    @note
        O(n)
*/
fa_buffer_t fa_buffer_dresize(size_t size_, fa_buffer_t buffer);

/** Destroy the given buffer. If the reference count is greater than zero,
    the actual destruction is postponed until the reference count reaches
    zero. See @ref fa_buffer_take_reference and @ref fa_buffer_release_reference.
    
    @note
        O(n)
*/
void fa_buffer_destroy(fa_buffer_t buffer);

/** Take a reference to the buffer, i.e. increase its reference count.
    
*/
void fa_buffer_take_reference(fa_buffer_t buffer);

/** Release a reference to the buffer, i.e. decrease its reference count.
    
*/
void fa_buffer_release_reference(fa_buffer_t buffer);

/** Return the size of the buffer.
    @note
        O(1)
*/
size_t fa_buffer_size(fa_buffer_t buffer);

/** Get the value of some meta-data attribute of the given buffer.
    @param buffer The buffer.
    @param string Attribute name (will be destroyed)
    @returns 
        The value (implementing @ref fa_dynamic).
*/
fa_ptr_t fa_buffer_get_meta(fa_buffer_t buffer, fa_string_t string);

/** Set the value of some meta-data attribute of the given buffer.
    @param buffer The buffer.
    @param string Attribute name.
    @param The value (implementing @ref fa_dynamic).
    
    @note
        Both the name and the value will be automatically destroyed
        when the buffer is destroyed or the key is removed from the map.
    
    
*/
void fa_buffer_set_meta(fa_buffer_t buffer,
                        fa_string_t string,
                        fa_ptr_t ptr);

/** Get all meta-data as a map from strings to values.
    @returns A @ref map of meta-data.
*/
fa_map_t fa_buffer_meta(fa_buffer_t buffer);

/** Get a value from the buffer.
    @note
        O(1)
*/
uint8_t fa_buffer_get(fa_buffer_t buffer, size_t size_);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set(fa_buffer_t buffer,
                   size_t size_,
                   uint8_t uInt8_);

/** Copy the first n bytes. 
*/
fa_buffer_t fa_buffer_take(size_t size_, fa_buffer_t buffer);

/** Copy from the given number of bytes. 
*/
fa_buffer_t fa_buffer_drop(size_t size_, fa_buffer_t buffer);

/** Efficient combination of take and drop. 
*/
fa_pair_t fa_buffer_take_drop(size_t size_, fa_buffer_t buffer);

/** Split into a list of buffers containing at most n bytes. 
*/
fa_list_t fa_buffer_split(fa_buffer_t buffer, size_t size_, bool copy);

/** Get a value from the buffer.
    @note
        O(1)
*/
int16_t fa_buffer_get_int16(fa_buffer_t buffer, size_t size_);

/** Get a value from the buffer.
    @note
        O(1)
*/
int32_t fa_buffer_get_int32(fa_buffer_t buffer, size_t size_);

/** Get a value from the buffer.
    @note
        O(1)
*/
int64_t fa_buffer_get_int64(fa_buffer_t buffer, size_t index);

/** Get a value from the buffer.
    @note
        O(1)
*/
float fa_buffer_get_float(fa_buffer_t buffer, size_t index);

/** Get a value from the buffer.
    @note
        O(1)
*/
double fa_buffer_get_double(fa_buffer_t buffer, size_t index);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_int16(fa_buffer_t buffer,
                         size_t index,
                         int16_t int16_);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_int32(fa_buffer_t buffer,
                         size_t index,
                         int32_t value);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_int64(fa_buffer_t buffer,
                         size_t index,
                         int64_t value);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_float(fa_buffer_t buffer,
                         size_t size_,
                         float float_);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_double(fa_buffer_t buffer,
                          size_t size_,
                          double double_);


fa_buffer_t fa_buffer_zip(fa_buffer_t buffer, fa_buffer_t buffer_);


fa_pair_t fa_buffer_unzip(fa_buffer_t buffer);

/**
    Reads a buffer from a file.

    @param path
        Path to the file to read.
    @return
        A new buffer.
*/
fa_buffer_t fa_buffer_read_raw(fa_string_t path);

/**
    Reads a buffer from a file, if the file is not bigger than max_size bytes.

    @param path
        Path to the file to read.
    @param max_size
        Maximum file size in bytes
    @return
        A new buffer, an error object, or NULL if the file was too big.
*/
fa_buffer_t fa_buffer_read_raw_max_size(fa_string_t path, size_t max_size);

/**
    Write a buffer to a file.

    @param path
        Path to the file to write.
*/
bool fa_buffer_write_raw(fa_string_t path, fa_buffer_t buffer);

/**
    Read an audio file.

    @param path
        Path to the file to read.
    @return
        A buffer or an error value.
*/
fa_buffer_t fa_buffer_read_audio(fa_string_t path);

/**
    Read an audio file no bigger than a certain size.

    @param path
        Path to the file to read.
    @param max_size
        Max size of the internal buffer, in bytes. Note that
        audio is represented internally by doubles, so to contain
        a 16-bit mono WAV file if 1 MB, max_size will have to be
        at least 4 MB. A max_size of 0 means no limit.
    @param crop
        Determines what happens when the audio file is too big.
        If crop is true, data is read from the audio file until
        max_size is reached. If crop is false, NULL is returned.
    @return
        A buffer, NULL (only when file is too big) or an error value.
*/
fa_buffer_t fa_buffer_read_audio_max_size(fa_string_t path, size_t max_size, bool crop);

/**
    Write an audio file.

    @param path
        Path to the file to read.
    @param buffer
        Buffer to write.
    @return
        The null pointer or an error value.
*/
fa_ptr_t fa_buffer_write_audio(fa_string_t path,
                               fa_buffer_t buffer);
                               
#ifdef FA_MP3_IMPORT
fa_buffer_t fa_buffer_read_mp3(fa_string_t path);
fa_buffer_t fa_buffer_read_mp3_max_size(fa_string_t path, size_t max_size, bool crop);
#endif

/** Resample the given buffer to the given rate (nullable).
    
    @param buffer
        Buffer to resample.
    @param sampleRate
        The new sample rate.
    @return
        A new buffer.
*/
fa_buffer_t fa_buffer_resample(double sampleRate,
                               fa_buffer_t buffer);

/** Return the address of the buffer.

    This function is unsafe as it provides access to the buffer contents without
    ownership semantics.
    @note
        O(1)
*/
void * fa_buffer_unsafe_address(fa_buffer_t buffer);

/** @}
    @}
    */

#endif // _FA_BUFFER

