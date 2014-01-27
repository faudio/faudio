
#ifndef _FA_IO
#define _FA_IO

#include <fa.h>
#include <fa/buffer.h>
#include <fa/atomic/ring_buffer.h>

/** @addtogroup FaIo

    Provides external streams.
    
    - *Sources* produce sequences of data (bytes, characters, floats etc)
    - *Sinks* consume sequences of data (bytes, characters, floats etc)
    - *Filters* transform sequences of data in an effectful manner
    - Both *sources* and *sinks* can be composed with filters.
    
    @since
        2.9
 
    @defgroup Fa Fa
    @{
    @defgroup FaIo Io
    @{
    */

/** Callback to receive data.
    
    Argument is either a buffer contaning a new chunk of data or `NULL`, indicating
    that the data source has been drained.
    
    The buffer may be of any size (including zero) and is valid for the duration of
    the callback. To retain data beyond this call the buffer should be copied, or its
    data extracted and written to some other destination.
*/
typedef void (* fa_io_callback_t)(fa_ptr_t, fa_buffer_t);

/** Implements the filter interface. 
*/
typedef struct _fa_io_filter_t * fa_io_filter_t;

/** Implements the filter interface (push calls have no effect, pull ignores the source). 
*/
typedef struct _fa_io_source_t * fa_io_source_t;

/** Implements the filter interface (pull calls have no effect, push ignores the sink). 
*/
typedef struct _fa_io_sink_t * fa_io_sink_t;

/**
*/
typedef struct {
            void (* pull)(fa_ptr_t,
                          fa_io_source_t,
                          fa_io_callback_t,
                          fa_ptr_t);
            void (* push)(fa_ptr_t, fa_io_sink_t, fa_buffer_t);
        } fa_io_filter_interface_t;


void fa_io_pull(fa_io_source_t source,
                fa_io_callback_t callback,
                fa_ptr_t ptr);


void fa_io_push(fa_io_sink_t sink, fa_buffer_t buffer);


void fa_io_pull_through(fa_io_filter_t filter,
                        fa_io_source_t source,
                        fa_io_callback_t callback,
                        fa_ptr_t ptr);


void fa_io_push_through(fa_io_filter_t filter,
                        fa_io_sink_t sink,
                        fa_buffer_t buffer);


fa_io_filter_t fa_io_compose(fa_io_filter_t filter,
                             fa_io_filter_t filter_);


fa_io_source_t fa_io_map(fa_io_source_t source,
                         fa_io_filter_t filter);


fa_io_sink_t fa_io_contramap(fa_io_filter_t filter,
                             fa_io_sink_t sink);


fa_io_filter_t fa_io_split(fa_io_sink_t sink);


fa_io_source_t fa_io_read_file(fa_string_t string);


fa_io_sink_t fa_io_write_file(fa_string_t string);


fa_io_source_t fa_io_standard_in();


fa_io_sink_t fa_io_standard_out();


fa_io_filter_t fa_io_identity();


fa_io_filter_t fa_io_create_ogg_encoder();


void fa_io_run(fa_io_source_t source, fa_io_sink_t sink);

/** @}
    @}
    */

#endif // _FA_IO

