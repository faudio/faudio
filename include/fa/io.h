
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

/** Generic I/O filter interface.
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
    compose(identity, f) = f
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

/** Create a simple stateful filter.
    The callback is invoked on push and the read callback on pull.
*/
fa_io_filter_t fa_io_create_simple_filter(fa_io_callback_t callback,
                                          fa_io_read_callback_t readCallback,
                                          fa_ptr_t ptr);
/** Create a simple stateful filter.
    The callback is invoked on push and the read callback on pull.
    The destructor is called just before the actual filter is destroyed.
*/
fa_io_filter_t fa_io_create_simple_filter_with_destructor(fa_io_callback_t callback,
                                                          fa_io_read_callback_t readCallback,
                                                          fa_ptr_t data,
                                                          fa_nullary_t destructor);

/** Create a filter that writes data passed through it to the given sink. 
*/
fa_io_filter_t fa_io_split(fa_io_sink_t sink);

/** Create source that reads from a file. 
    The path is consumed.
*/
fa_io_source_t fa_io_read_file(fa_string_t path);

/** Create source that reads from a file. 
*/
fa_io_source_t fa_io_read_file_between(fa_string_t path, fa_ptr_t start, fa_ptr_t end);

/** Create source that reads audio data from an audio file. 
    The path is consumed.
*/
fa_io_source_t fa_io_read_audio_file(fa_string_t path);

/** Create source that reads audio data from an audio file. 
*/
fa_io_source_t fa_io_read_audio_file_between(fa_string_t path, fa_ptr_t startFrames, fa_ptr_t endFrames);

/** Create sink that writes to a file. 
*/
fa_io_sink_t fa_io_write_file(fa_string_t string);

/** Create source that reads from the standard input. 
*/
fa_io_source_t fa_io_standard_in();

/** Create sink that writes to the standard output. 
*/
fa_io_sink_t fa_io_standard_out();

/** Create a source from a buffer. 
*/
fa_io_source_t fa_io_from_buffer(fa_buffer_t buffer);

/** Create a source from a ring buffer. 
*/
fa_io_source_t fa_io_from_ring_buffer(fa_atomic_ring_buffer_t ringBuffer);

/** Create an encoder to the `ogg/vorbis` format.
    @warning
        For now requires input to be mono, 44100, 64-bit floating.
      
*/
fa_io_filter_t fa_io_create_ogg_encoder(long sampleRate, long channels);

/** Continously data from the given source and push it into the sink.

    This function blocks until the given source is exhausted. If this
    never happens, this function blocks forever.
*/
void fa_io_run(fa_io_source_t source, fa_io_sink_t sink);

/** Continously pull data from the given source and store it into a buffer.

    This function blocks until the given source is exhausted. If this
    never happens, this function blocks forever, or until the buffer
    cannot grow because there is no more memory.
 */
fa_buffer_t fa_io_pull_to_buffer(fa_io_source_t source);


/** @}
    @}
    */

#endif // _FA_IO

