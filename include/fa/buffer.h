
#ifndef _FA_BUFFER
#define _FA_BUFFER

#include <fa.h>
#include <fa/std.h>
#include <fa/pair.h>
#include <fa/string.h>
#include <fa/type.h>

/** @addtogroup FaBuffer
 
    @addtogroup FaBuffer
    
    Mutable buffers. 
    
    Each buffer is a mutable sequence of bytes with single-ownership
    semantics. The length of a buffer can never change, but its memory can be reused using the destructive
    resize operation.
    
    Note that buffers are not collections as their contents are mutable.


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

/** Copy the given buffer using the given size.
    @note
        O(n)
*/
fa_buffer_t fa_buffer_resize(size_t, fa_buffer_t);

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

/** Read a value from the buffer.
    @note
        O(1)
*/
uint8_t fa_buffer_get(fa_buffer_t, size_t);

/** Update a value in the buffer.
    @note
        O(1)
*/
void fa_buffer_set(fa_buffer_t, size_t, uint8_t);


int16_t fa_buffer_get_int16(fa_buffer_t, size_t);


int32_t fa_buffer_get_int32(fa_buffer_t, size_t);


int64_t fa_buffer_get_int64(fa_buffer_t, size_t);


float fa_buffer_get_float(fa_buffer_t, size_t);


double fa_buffer_get_double(fa_buffer_t, size_t);


void fa_buffer_set_int16(fa_buffer_t, size_t, int16_t);


void fa_buffer_set_int32(fa_buffer_t, size_t, int32_t);


void fa_buffer_set_int64(fa_buffer_t, size_t, int64_t);


void fa_buffer_set_float(fa_buffer_t, size_t, float);


void fa_buffer_set_double(fa_buffer_t, size_t, double);


fa_buffer_t fa_buffer_read_raw(fa_string_t);


void fa_buffer_write_raw(fa_string_t, fa_buffer_t);

/**
    Reads an audio file.

    @returns
        A pair $$type, buffer)$$.
*/
fa_pair_t fa_buffer_read_audio(fa_string_t);


fa_ptr_t fa_buffer_write_audio(fa_string_t, fa_type_t, fa_buffer_t);

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

