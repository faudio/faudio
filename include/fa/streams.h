
#ifndef _FA_STREAMS
#define _FA_STREAMS

#include <fa.h>
#include <fa/buffer.h>

/** @addtogroup FaStreams

    Provides external streams.

    @since
        2.9
 
    @defgroup Fa Fa
    @{
    @defgroup FaStreams Streams
    @{
    */


typedef struct {
            bool (* more)(fa_ptr_t);
        } fa_handle_interface_t;


typedef struct {
            void (* read)(fa_ptr_t, fa_buffer_t);
            void (* reading)(fa_source_t,
                             size_t (*)(fa_ptr_t, fa_buffer_t),
                             fa_ptr_t);
        } fa_source_interface_t;

/** A value of an unknown type implementing @ref fa_handle_interface_t.
*/
typedef struct _fa_handle_t * fa_handle_t;

/** A value of an unknown type implementing @ref fa_source_interface_t.
*/
typedef struct _fa_source_t * fa_source_t;

/** Whether a source or sink is open.
    
    If `false`, all subsequent call to `read` or `write` does nothing.
*/
bool fa_is_open(fa_handle_t handle);

/** Whether a source or sink is open.
    
    If `true`, all subsequent call to `read` or `write` does nothing.
*/
bool fa_is_closed(fa_handle_t handle);

/** Read from a source (convenience function).

    The contents of the buffer are overwritten and the number of written bytes,
    which is at most `fa_buffer_size(buffer)` is returned.
*/
size_t fa_source_read(fa_source_t source, fa_buffer_t buffer);


typedef size_t (* fa_read_callback_t)(fa_ptr_t, fa_buffer_t);

/** Read from a source.

    The contents of the buffer are overwritten and the number of written bytes,
    which is at most `fa_buffer_size(buffer)` is returned.
*/
void fa_source_reading(fa_source_t source,
                       fa_read_callback_t readCallback,
                       fa_ptr_t ptr);

/** @}
    @}
    */

#endif // _FA_STREAMS

