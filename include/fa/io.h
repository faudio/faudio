
#ifndef _FA_IO
#define _FA_IO

#include <fa.h>
#include <fa/buffer.h>

/** @addtogroup FaIo

    @since
        2.9
 
    @defgroup Fa Fa
    @{
    @defgroup FaIo Io
    @{
    */


typedef struct {
            void (* read)(fa_ptr_t, fa_buffer_t); bool (* more)(fa_ptr_t);
        } fa_io_source_interface_t;


typedef struct {
            void (* write)(fa_ptr_t, fa_buffer_t); void (* close)(fa_ptr_t);
        } fa_io_sink_interface_t;


typedef struct _fa_io_source_t * fa_io_source_t;


typedef struct _fa_io_sink_t * fa_io_sink_t;


void fa_io_read(fa_io_source_t, fa_buffer_t);


bool fa_io_more(fa_io_source_t);


void fa_io_write(fa_io_sink_t, fa_buffer_t);


void fa_io_close(fa_io_sink_t);

/** @}
    @}
    */

#endif // _FA_IO

