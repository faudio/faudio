
#ifndef _DOREMIR_SIGNAL
#define _DOREMIR_SIGNAL

#include <doremir.h>
#include <doremir/type.h>
#include <doremir/processor.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirSignal Signal
    @{
    */

typedef struct _doremir_signal_t * doremir_signal_t;
doremir_type_t doremir_signal_get_type(doremir_signal_t);
doremir_signal_t doremir_signal_apply(doremir_processor_t,
                                      doremir_signal_t);
doremir_signal_t doremir_signal_apply2(doremir_processor_t,
                                       doremir_signal_t,
                                       doremir_signal_t);
doremir_signal_t doremir_signal_apply3(doremir_processor_t,
                                       doremir_signal_t,
                                       doremir_signal_t,
                                       doremir_signal_t);

/** @}
    @}
    */

#endif // _DOREMIR_SIGNAL

