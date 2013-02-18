
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

typedef double (* doremir_plot_function_t)(doremir_ptr_t,
                                           int,
                                           double,
                                           double);
void doremir_plot_use_gnu();
void doremir_plot_use_core();
void doremir_plot_discrete(doremir_list_t);
void doremir_plot_continous(doremir_plot_function_t,
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

