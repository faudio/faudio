
#ifndef _FA_IO
#define _FA_IO

#include <fa.h>
#include <fa/buffer.h>
#include <fa/atomic/ring_buffer.h>

/** @addtogroup FaIo

    Provides external streams.
    
    - *Sources* produce sequences of data (bytes, characters, floats etc)
    - *Sinks* consume sequences of data (bytes, characters, floats etc)
    - Both *sources* and *sinks* accepts external buffers.
    
    @since
        2.9
 
    @defgroup Fa Fa
    @{
    @defgroup FaIo Io
    @{
    */

/** Callback to receive data.
    
    Argument is either a (possibly empty) buffer, meaning that more data is to arrive,
    or `NULL`, indicating that the data source has been extinguished.
*/
typedef void (* fa_io_buffer_callback_t)(fa_buffer_t);


typedef struct {
            void pull(fa_ptr_t, fa_io_source_t, fa_io_buffer_callback_t);
        } fa_io_filter_interface_t;

/** Implements the filter interface. 
*/
typedef struct _fa_io_filter_t * fa_io_filter_t;

/** Implements the filter interface (ignores the source). 
*/
typedef struct _fa_io_source_t * fa_io_source_t;

/** Filter two filterable things.
*/
fa_ptr_t fa_io_filter(fa_ptr_t ptr, fa_ptr_t ptr_);

/** @}
    @}
    */

#endif // _FA_IO

