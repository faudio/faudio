
#ifndef _FA_BUFFER
#define _FA_BUFFER

#include <fa.h>
#include <fa/std.h>
#include <fa/pair.h>
#include <fa/string.h>

/** @addtogroup FaBuffer
 
    Mutable buffers. 
    
    Each buffer is a mutable sequence of bytes with single-ownership semantics. The length
    of a buffer can never change, but its memory can be reused using the destructive
    resize operation.
    
    TODO formalize bounds checking etc.

    @par Literals
    - `buffer(size)` 

    @par Implements 
    - fa_copy_t
    - fa_destroy_t
    - fa_string_show_t

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

/** Create a new buffer.
    @note
        O(n)
*/
fa_buffer_t fa_buffer_create(size_t);

/** Create a buffer wrapping the given memory region.

    @param ptr  Pointer to wrap.
    @param size Number of bytes to wrap.
    @param destroy_function Function to call upon destruction (optional).
    @param destroy_data Data closed over by the destroy function.
    @note
        O(1)
*/
fa_buffer_t fa_buffer_wrap(fa_ptr_t, size_t, fa_unary_t, fa_ptr_t);

/** Copy the given buffer.
    @note
        O(n)
*/
fa_buffer_t fa_buffer_copy(fa_buffer_t);

/** Return a new buffer of the given size containing the contents
    of the given buffer padded with zero bytes at the end.
    @note
        O(n)
*/
fa_buffer_t fa_buffer_resize(size_t, fa_buffer_t);

/** Return a new buffer of the given size containing the contents
    of the given buffer padded with zero bytes at the end.
    
    The given buffer is destroyed. This function can reuse memory
    from the destroyed buffer and be considerably faster than @ref fa_buffer_resize.
    
    @note
        O(n)
*/
fa_buffer_t fa_buffer_dresize(size_t, fa_buffer_t);

/** Destroy the given buffer.
    @note
        O(n)
*/
void fa_buffer_destroy(fa_buffer_t);

/** Return the size of the buffer.
    @note
        O(1)
*/
size_t fa_buffer_size(fa_buffer_t);

/** Get a value from the buffer.
    @note
        O(1)
*/
uint8_t fa_buffer_get(fa_buffer_t, size_t);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set(fa_buffer_t, size_t, uint8_t);

/** Get a value from the buffer.
    @note
        O(1)
*/
int16_t fa_buffer_get_int16(fa_buffer_t, size_t);

/** Get a value from the buffer.
    @note
        O(1)
*/
int32_t fa_buffer_get_int32(fa_buffer_t, size_t);

/** Get a value from the buffer.
    @note
        O(1)
*/
int64_t fa_buffer_get_int64(fa_buffer_t, size_t);

/** Get a value from the buffer.
    @note
        O(1)
*/
float fa_buffer_get_float(fa_buffer_t, size_t);

/** Get a value from the buffer.
    @note
        O(1)
*/
double fa_buffer_get_double(fa_buffer_t, size_t);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_int16(fa_buffer_t, size_t, int16_t);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_int32(fa_buffer_t, size_t, int32_t);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_int64(fa_buffer_t, size_t, int64_t);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_float(fa_buffer_t, size_t, float);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set_double(fa_buffer_t, size_t, double);

/**
    Reads a buffer from a file.

    @param path
        Path to the file to read.
    @return
        A new buffer.
*/
fa_buffer_t fa_buffer_read_raw(fa_string_t);

/**
    Write a buffer to a file.

    @param path
        Path to the file to write.
*/
void fa_buffer_write_raw(fa_string_t, fa_buffer_t);

/**
    Read an audio file.

    @param path
        Path to the file to read.
    @return
        A `(Channels, Buffer)` pair.
*/
fa_pair_t fa_buffer_read_audio(fa_string_t);

/**
    Write an audio file.

    @param path
        Path to the file to read.
    @param channels
        Number of channels.
    @param buffer
        Buffer to write.
    @return
        The null pointer or an error value.
*/
fa_ptr_t fa_buffer_write_audio(fa_string_t, int, fa_buffer_t);

/** Return the address of the buffer.

    This function is unsafe as it provides access to the buffer contents without
    ownership semantics.
    @note
        O(1)
*/
void * fa_buffer_unsafe_address(fa_buffer_t);

/** @}
    @}
    */

#endif // _FA_BUFFER

