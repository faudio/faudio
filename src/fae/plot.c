
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/plot.h>
#include <fae/thread.h>
#include <fae/util.h>

typedef fae_plot_function_t plot_func;

enum plot_backend {
    gnu_plot,
    core_plot
};

enum plot_backend   plot_backend_g  = core_plot;

void fae_audio_engine_log_info(fae_string_t);
void run_core_plot(plot_func func, ptr_t funcData, nullary_t cont, ptr_t contData);
void run_gnu_plot(plot_func func, ptr_t funcData, nullary_t cont, ptr_t contData);


/** Use gnuplot for plotting.
 */
void fae_plot_use_gnu()
{
    plot_backend_g = gnu_plot;
    fae_audio_engine_log_info(string("Using gnuplot backend"));
}

/** Use CorePlot for plotting.
 */
void fae_plot_use_core()
{
    plot_backend_g = core_plot;
    fae_audio_engine_log_info(string("Using CorePlot backend"));
}

/** Run a plot of the given functions.
    @param func         Function defining the plot.
    @param data         Value to be passed to the plot function.
    @param cont         Continuation function.
    @param cont_data    Value to be passed to the continuation function.
 */
void fae_plot_continous
(
    plot_func           func,
    fae_ptr_t       data,
    fae_nullary_t   cont,
    fae_ptr_t       cont_data
)
{
    switch (plot_backend_g) {
    case gnu_plot:
        run_gnu_plot(func, data, cont, cont_data);
        return;

    case core_plot:
        run_core_plot(func, data, cont, cont_data);
        return;
    }
}

#define PLOTTER(T) \
    double plot_##T(void *data, int i, double t, double x)      \
    {                                                           \
        fae_buffer_t buf = data;                            \
                                                                \
        size_t  sz = fae_buffer_size(buf) / sizeof(T);      \
        T     * ds = fae_buffer_unsafe_address(buf);        \
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
void fae_plot_buffer_float(fae_buffer_t  buffer,
                               fae_nullary_t cont,
                               fae_ptr_t     data)
{
    fae_plot_continous(plot_float, buffer, cont, data);
}

/** Run a plot on the given buffer, treating its contents as
    64-bit floating point data.
 */
void fae_plot_buffer_double(fae_buffer_t      buffer,
                                fae_nullary_t     cont,
                                fae_ptr_t         data)
{
    fae_plot_continous(plot_double, buffer, cont, data);
}

