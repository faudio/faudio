
#ifndef _DOREMIR_PLOT
#define _DOREMIR_PLOT

#include <doremir.h>
#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPlot Plot
    @{
    */

typedef double (* doremir_plot_func_t)(doremir_ptr_t,
                                       int,
                                       double,
                                       double);
void doremir_plot_show(doremir_plot_func_t,
                       doremir_ptr_t,
                       doremir_nullary_t,
                       doremir_ptr_t);

/** @}
    @}
    */

#endif // _DOREMIR_PLOT

