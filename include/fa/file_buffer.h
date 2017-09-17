
#ifndef _FA_FILE_BUFFER
#define _FA_FILE_BUFFER

#include <fa.h>
#include <fa/std.h>
#include <fa/pair.h>
#include <fa/string.h>
#include <fa/map.h>

/** @addtogroup FaFileBuffer
 
    Read-only virtual file buffer.
    
    
    
    @par Literals
    - `buffer(size)` 

    @par Implements 
    - fa_destroy_t
    - fa_string_show_t
    - fa_reference_count
    - fa_meta_data

    @see 
    - [Data structures](@ref DataStructures)
 
    @defgroup Fa Fa
    @{
    @defgroup FaFileBuffer FileBuffer
    @{
    */

/** @typedef fa_file_buffer_t
    The file buffer. 
*/
typedef struct _fa_file_buffer_t * fa_file_buffer_t;

/** Create a new file buffer.
    @param path
        A pathname.
    @param buffer_size
        Size of the in-memory buffer.
    @return
        A file_buffer
    @note
        O(n)
*/
fa_file_buffer_t fa_file_buffer_create(fa_string_t path, size_t buffer_size);

/** Destroy the given buffer. If the reference count is greater than zero,
    the actual destruction is postponed until the reference count reaches
    zero. See @ref fa_file_buffer_take_reference and @ref fa_file_buffer_release_reference.
    
    @note
        O(n)
*/
void fa_file_buffer_destroy(fa_file_buffer_t buffer);

/** Take a reference to the buffer, i.e. increase its reference count.
    
*/
void fa_file_buffer_take_reference(fa_file_buffer_t buffer);

/** Release a reference to the buffer, i.e. decrease its reference count.
    
*/
void fa_file_buffer_release_reference(fa_file_buffer_t buffer);

/** Return the size of the buffer.
    @note
        O(1)
*/
size_t fa_file_buffer_buffer_size(fa_file_buffer_t buffer);

/** Return the "virtual size" of the file. If the file is opened with
    fa_file_buffer_create, this is the actual file size.
    However if the file_buffer is created with fa_file_buffer_read_audio,
    the file_size returned is the size of the uncompressed audio
    contents of the file, excluding any headers.
    @note
        O(1)
*/
size_t fa_file_buffer_file_size(fa_file_buffer_t file_buffer);

/** Return the file path of the buffer.
*/
fa_string_t fa_file_buffer_path(fa_file_buffer_t buffer);

/** Get the value of some meta-data attribute of the given buffer.
    @param buffer The buffer.
    @param string Attribute name (will be destroyed)
    @returns 
        The value (implementing @ref fa_dynamic).
*/
fa_ptr_t fa_file_buffer_get_meta(fa_file_buffer_t buffer, fa_string_t string);

/** Set the value of some meta-data attribute of the given buffer.
    @param buffer The buffer.
    @param string Attribute name.
    @param The value (implementing @ref fa_dynamic).
    
    @note
        Both the name and the value will be automatically destroyed
        when the buffer is destroyed or the key is removed from the map.
    
    
*/
void fa_file_buffer_set_meta(fa_file_buffer_t buffer,
                        fa_string_t string,
                        fa_ptr_t ptr);

/** Get all meta-data as a map from strings to values.
    @returns A @ref map of meta-data.
*/
fa_map_t fa_file_buffer_meta(fa_file_buffer_t buffer);

/** Get a value from the buffer.
    @note
        O(1)
*/
uint8_t fa_file_buffer_get(fa_file_buffer_t buffer, size_t size_);


/** Get a value from the buffer.
    @note
        O(1)
*/
double fa_file_buffer_get_double(fa_file_buffer_t buffer, size_t index);

/** Get a value from the buffer.
    @note
        O(1)
*/
float fa_file_buffer_get_float(fa_file_buffer_t buffer, size_t index);


/**
    Write a buffer to a file.

    @param path
        Path to the file to write.
*/
bool fa_file_buffer_write_raw(fa_string_t path, fa_file_buffer_t buffer);

/**
    Read an audio file.

    @param path
        Path to the file to read.
    @return
        A buffer or an error value.
*/
fa_file_buffer_t fa_file_buffer_read_audio(fa_string_t path, size_t buffer_size, fa_sample_type_t sample_type);

#ifdef FA_MP3_IMPORT
fa_file_buffer_t fa_file_buffer_read_mp3(fa_string_t path, size_t buffer_size, fa_sample_type_t sample_type);
#endif


size_t fa_file_buffer_seek(fa_file_buffer_t file_buffer, size_t pos);

size_t fa_file_buffer_seek_if_needed(fa_file_buffer_t file_buffer, size_t pos);

void fa_file_buffer_hint(fa_file_buffer_t file_buffer, size_t offset);

/** Return the address of the buffer.

    This function is unsafe as it provides access to the buffer contents without
    ownership semantics.
    @note
        O(1)
*/
void * fa_file_buffer_unsafe_address(fa_file_buffer_t buffer);

size_t fa_file_buffer_frames(fa_file_buffer_t file_buffer);

size_t fa_file_buffer_sample_rate(fa_file_buffer_t file_buffer);

size_t fa_file_buffer_channels(fa_file_buffer_t file_buffer);

/** @}
    @}
    */

#endif // _FA_FILE_BUFFER

