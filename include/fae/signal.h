
#ifndef _FAE_SIGNAL
#define _FAE_SIGNAL

#include <fae.h>
#include <fae/pair.h>
#include <fae/type.h>
#include <fae/time.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaeSignal Signal
    @{
    */

typedef struct _fae_signal_t * fae_signal_t;
fae_signal_t fae_signal_constant(fae_ptr_t);
fae_signal_t fae_signal_identity();
fae_signal_t fae_signal_lift(fae_unary_t, fae_ptr_t);
fae_signal_t fae_signal_lift2(fae_binary_t, fae_ptr_t);
fae_signal_t fae_signal_time();
fae_signal_t fae_signal_delay(fae_time_t, fae_signal_t);
fae_signal_t fae_signal_fix(fae_signal_t (*)(fae_ptr_t,
                                             fae_signal_t),
                            fae_ptr_t);

/** @}
    @}
    */

#endif // _FAE_SIGNAL

