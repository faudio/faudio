
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
        } fa_source_interface_t;


typedef struct {
            void (* write)(fa_ptr_t, fa_buffer_t); void (* close)(fa_ptr_t);
        } fa_sink_interface_t;


typedef struct _fa_source_t * fa_source_t;


typedef struct _fa_sink_t * fa_sink_t;


void fa_source_read(fa_source_t source, fa_buffer_t buffer);


bool fa_source_more(fa_source_t source);


void fa_sink_write(fa_sink_t sink, fa_buffer_t buffer);


void fa_sink_close(fa_sink_t sink);

/** @}
    @}
    */

#endif // _FA_STREAMS

