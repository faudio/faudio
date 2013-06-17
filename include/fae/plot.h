
#ifndef _FAE_PLOT
#define _FAE_PLOT

#include <fae.h>
#include <fae/std.h>
#include <fae/buffer.h>

/** @defgroup Fae Fae
    @{
    @defgroup FaePlot Plot
    @{
    */

typedef double (* fae_plot_function_t)(fae_ptr_t,
                                       int,
                                       double,
                                       double);
void fae_plot_use_gnu();
void fae_plot_use_core();
void fae_plot_continous(fae_plot_function_t,
                        fae_ptr_t,
                        fae_nullary_t,
                        fae_ptr_t);
void fae_plot_buffer_float(fae_buffer_t, fae_nullary_t, fae_ptr_t);
void fae_plot_buffer_double(fae_buffer_t,
                            fae_nullary_t,
                            fae_ptr_t);

/** @}
    @}
    */

#endif // _FAE_PLOT

