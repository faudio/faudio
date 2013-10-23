
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/plot.h>
#include <fa/thread.h>
#include <fa/util.h>

typedef fa_plot_function_t plot_func;

enum plot_backend {
    gnu_plot
};

enum plot_backend plot_backend_g = gnu_plot;

void fa_fa_log_info(fa_string_t);
void run_gnu_plot(plot_func func, ptr_t funcData, nullary_t cont, ptr_t contData);


/** Use gnuplot for plotting.
 */
void fa_plot_use_gnu()
{
    plot_backend_g = gnu_plot;
    fa_fa_log_info(string("Using gnuplot backend"));
}

/** Run a plot of the given functions.
    @param func         Function defining the plot.
    @param data         Value to be passed to the plot function.
    @param cont         Continuation function.
    @param cont_data    Value to be passed to the continuation function.
 */
void fa_plot_continous
(
    plot_func           func,
    fa_ptr_t       data,
    fa_nullary_t   cont,
    fa_ptr_t       cont_data
)
{
    switch (plot_backend_g) {
    case gnu_plot:
        run_gnu_plot(func, data, cont, cont_data);
        return;

        assert(false && "Unknown plot backend.");
    }
}

#define PLOTTER(T) \
    double plot_##T(void *data, int i, double t, double x)      \
    {                                                           \
        fa_buffer_t buf = data;                                 \
                                                                \
        size_t  sz = fa_buffer_size(buf) / sizeof(T);           \
        T     * ds = fa_buffer_unsafe_address(buf);             \
                                                                \
        if (i == 0) {                                           \
            return ds[((size_t)(sz * ((x + 1) / 2)))];          \
        } else {                                                \
            return -2;                                          \
        }                                                       \
    }                                                           \
 
PLOTTER(float);
PLOTTER(double);

/** Run a plot on the given buffer, treating its contents as
    32-bit floating point data.
 */
void fa_plot_buffer_float(fa_buffer_t  buffer,
                          fa_nullary_t cont,
                          fa_ptr_t     data)
{
    fa_plot_continous(plot_float, buffer, cont, data);
}

/** Run a plot on the given buffer, treating its contents as
    64-bit floating point data.
 */
void fa_plot_buffer_double(fa_buffer_t      buffer,
                           fa_nullary_t     cont,
                           fa_ptr_t         data)
{
    fa_plot_continous(plot_double, buffer, cont, data);
}

