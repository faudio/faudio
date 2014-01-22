
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
            void (* read)(fa_ptr_t, fa_buffer_t); bool (* more)(fa_ptr_t);
        } fa_streams_source_interface_t;


typedef struct {
            void (* write)(fa_ptr_t, fa_buffer_t); void (* close)(fa_ptr_t);
        } fa_streams_sink_interface_t;


typedef struct _fa_streams_source_t * fa_streams_source_t;


typedef struct _fa_streams_sink_t * fa_streams_sink_t;


void fa_streams_read(fa_streams_source_t, fa_buffer_t);


bool fa_streams_more(fa_streams_source_t);


void fa_streams_write(fa_streams_sink_t, fa_buffer_t);


void fa_streams_close(fa_streams_sink_t);

/** @}
    @}
    */

#endif // _FA_STREAMS

