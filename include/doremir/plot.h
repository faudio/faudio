
#ifndef _DOREMIR_PLOT
#define _DOREMIR_PLOT

#include <doremir.h>
#include <doremir/std.h>
#include <doremir/buffer.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPlot Plot
    @{
    */

typedef double (* doremir_plot_func_t)(doremir_ptr_t,
                                       int,
                                       double,
                                       double);
void doremir_plot_use_gnu_plot();
void doremir_plot_use_core_plot();
void doremir_plot_func(doremir_plot_func_t,
                       doremir_ptr_t,
                       doremir_nullary_t,
                       doremir_ptr_t);
void doremir_plot_buffer_float(doremir_buffer_t,
                               doremir_nullary_t,
                               doremir_ptr_t);
void doremir_plot_buffer_double(doremir_buffer_t,
                                doremir_nullary_t,
                                doremir_ptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_PLOT

