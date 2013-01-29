
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/plot.h>
#include <doremir/thread.h>
#include <doremir/util.h>

typedef doremir_plot_func_t plot_func;

void run_core_plot (plot_func func, ptr_t funcData, nullary_t cont, ptr_t contData);
void run_gnu_plot (plot_func func, ptr_t funcData, nullary_t cont, ptr_t contData);

enum plot_backend {
    gnu_plot,
    core_plot
};

/** Run a plot of the given functions.
 */
void doremir_plot_func
(
  plot_func           func,
  doremir_ptr_t       data,
  doremir_nullary_t   cont,
  doremir_ptr_t       cont_data
)
{
    switch (core_plot)
    {
        case gnu_plot:
            run_gnu_plot(func, data, cont, cont_data);
            return;
        case core_plot:
            run_core_plot(func, data, cont, cont_data);
            return;
    }
}  

#define PLOTTER(T) \
    double plot_##T(void * ct, int i, double t, double x)       \
    {                                                           \
        doremir_buffer_t buf = ct;                              \
                                                                \
        size_t  sz = doremir_buffer_size(buf) / sizeof(T);      \
        T     * ds = doremir_buffer_unsafe_address(buf);        \
                                                                \
        if (i == 0) {                                           \
            return ds[((size_t)(sz * ((x + 1) / 2)))];          \
        } else if (i == 1) {                                    \
            return ds[((size_t)(sz * ((x + 1) / 2)))] * -1;     \
        } else {                                                \
            return -2;                                          \
        }                                                       \
    }                                                           \

PLOTTER(float);
PLOTTER(double);

/** Run a plot on the given buffer, treating its contents as
    32-bit floating point data.
 */
void doremir_plot_buffer_float(doremir_buffer_t  buffer,
                               doremir_nullary_t cont,
                               doremir_ptr_t     data)
{
    doremir_plot_func(plot_float, buffer, cont, data);
}

/** Run a plot on the given buffer, treating its contents as
    64-bit floating point data.
 */
void doremir_plot_buffer_double(doremir_buffer_t      buffer,
                                doremir_nullary_t     cont,
                                doremir_ptr_t         data)
{
    doremir_plot_func(plot_double, buffer, cont, data);
}

