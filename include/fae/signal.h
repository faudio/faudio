
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
typedef fae_signal_t (* fae_signal_unary_signal_t)(fae_ptr_t,
                                                   fae_signal_t);
typedef double (* fae_signal_unary_double_t)(fae_ptr_t, double);
typedef double (* fae_signal_binary_double_t)(fae_ptr_t,
                                              double,
                                              double);
fae_signal_t fae_signal_time();
fae_signal_t fae_signal_random();
fae_signal_t fae_signal_constant(double);
fae_signal_t fae_signal_lift(fae_signal_unary_double_t (fae_string_t),
                             fae_ptr_t,
                             fae_signal_t);
fae_signal_t fae_signal_lift2(fae_signal_binary_double_t (fae_string_t),
                              fae_ptr_t,
                              fae_signal_t,
                              fae_signal_t);
fae_signal_t fae_signal_loop(fae_signal_unary_signal_t, fae_ptr_t);
fae_signal_t fae_signal_delay(int, fae_signal_t);
fae_signal_t fae_signal_input(int);
fae_signal_t fae_signal_output(int, int, fae_signal_t);

/** @}
    @}
    */

#endif // _FAE_SIGNAL

