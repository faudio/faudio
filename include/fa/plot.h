
#ifndef _FA_PLOT
#define _FA_PLOT

#include <fa.h>
#include <fa/std.h>
#include <fa/buffer.h>

/** @defgroup Fa Fa
    @{
    @defgroup FaPlot Plot
    @{
    */

typedef double (* fa_plot_function_t)(fa_ptr_t,
                                      int,
                                      double,
                                      double);
void fa_plot_use_gnu();
void fa_plot_use_core();
void fa_plot_continous(fa_plot_function_t,
                       fa_ptr_t,
                       fa_nullary_t,
                       fa_ptr_t);
void fa_plot_buffer_float(fa_buffer_t, fa_nullary_t, fa_ptr_t);
void fa_plot_buffer_double(fa_buffer_t, fa_nullary_t, fa_ptr_t);

/** @}
    @}
    */

#endif // _FA_PLOT

