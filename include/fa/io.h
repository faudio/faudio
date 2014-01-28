
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

/** Callback to recive callback to receive data.
*/
typedef void (* fa_io_read_callback_t)(fa_ptr_t,
                                       fa_io_callback_t,
                                       fa_ptr_t);

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

/** Pull data from a source. 
*/
void fa_io_pull(fa_io_source_t source,
                fa_io_callback_t callback,
                fa_ptr_t ptr);

/** Push data to a sink. 
*/
void fa_io_push(fa_io_sink_t sink, fa_buffer_t buffer);

/** Pull data from a source after passing it through the given filter. 
*/
void fa_io_pull_through(fa_io_filter_t filter,
                        fa_io_source_t source,
                        fa_io_callback_t callback,
                        fa_ptr_t ptr);

/** Push data to a sink after passing it through the given filter. 
*/
void fa_io_push_through(fa_io_filter_t filter,
                        fa_io_sink_t sink,
                        fa_buffer_t buffer);

/** Create filter that passes through its input unchanged.
    Forms a monoid with identity so
    
    ~~~
    compose(f, compose (g, h)) = compose(compose(f, g), h)
    compose(identity,f) = f
    compose(g, identity) = g
    ~~~
    
    
*/
fa_io_filter_t fa_io_identity();

/** Compose two filters. 
*/
fa_io_filter_t fa_io_compose(fa_io_filter_t filter,
                             fa_io_filter_t filter_);

/** Apply a filter to the output of a source. 
*/
fa_io_source_t fa_io_apply(fa_io_source_t source,
                           fa_io_filter_t filter);

/** Apply a filter to the input of a sink. 
*/
fa_io_sink_t fa_io_coapply(fa_io_filter_t filter,
                           fa_io_sink_t sink);

/** Create a simple stateful  filter.
    The callback is invoked on push and the read callback on pull.
*/
fa_io_filter_t fa_io_create_simple_filter(fa_io_callback_t callback,
                                          fa_io_read_callback_t readCallback,
                                          fa_ptr_t ptr);

/** Create a filter that writes data passed through it to the given sink. 
*/
fa_io_filter_t fa_io_split(fa_io_sink_t sink);

/** Create source that reads from a file. 
*/
fa_io_source_t fa_io_read_file(fa_string_t string);

/** Create source that writes to a file. 
*/
fa_io_sink_t fa_io_write_file(fa_string_t string);

/** Create source that reads from the standard input. 
*/
fa_io_source_t fa_io_standard_in();

/** Create source that reads to the standfard output. 
*/
fa_io_sink_t fa_io_standard_out();


fa_io_filter_t fa_io_create_ogg_encoder();


void fa_io_run(fa_io_source_t source, fa_io_sink_t sink);

/** @}
    @}
    */

#endif // _FA_IO

