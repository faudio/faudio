
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/plot.h>
#include <doremir/thread.h>
#include <doremir/directory.h>
#include <doremir/util.h>

typedef doremir_plot_function_t plot_func_t;

#define samples_k     500000
#define down_sample_k 10
#define plot_format_k \
    "set title 'Audio Engine Plot'                                                          \n" \
    "set xrange [-1:1]                                                                      \n" \
    "set yrange [-1:1]                                                                      \n" \
    "set size 1.0, 1.0                                                                      \n" \
    "set terminal postscript landscape enhanced mono lw 1 'Helvetica' 14                    \n" \
    "set output '%2$s.ps'                                                                   \n" \
    "set zeroaxis                                                                           \n" \
    "plot '%1$s' using 1:2 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 1',    \\\n" \
    "     '%1$s' using 1:3 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 2',    \\\n" \
    "     '%1$s' using 1:4 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 3',    \\\n" \
    "     '%1$s' using 1:5 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 4',    \\\n" \
    "     '%1$s' using 1:6 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 5'       \n"

void generate_plot_file(plot_func_t func, ptr_t func_data, 
                        char* out_res, char* plot_res)
{
    char dat[L_tmpnam], plot[L_tmpnam], out_dir[100], out[100];
    char *home;
    
    home = unstring(doremir_directory_home());
    tmpnam(dat);
    tmpnam(plot);
    sprintf(out_dir, "%s/.doremiraudio", home);
    sprintf(out,     "%s/plot", out_dir);

    inform(string_dappend(string("Creating "), string_dappend(string(out), string(".ps"))));
    
    doremir_directory_create(string(out_dir));

    // Write data and plot file
    FILE *datf = fopen(dat, "w+");
    FILE *plotf = fopen(plot, "w+");

    for (int sample = 0; sample < samples_k; ++sample) {
        double x = ((double) sample) / ((double) samples_k) * 2 - 1;
        double ys[5];
        for (int index = 0; index < 5; ++index) {
            ys[index] = func(func_data, index, 0, x);
        }
        fprintf(datf, "%f %f %f %f %f %f \n", 
            x, ys[0], ys[1], ys[2], ys[3], ys[4]);
    }
    fprintf(plotf, plot_format_k, dat, out, down_sample_k);
    fflush(datf);
    fflush(plotf);

    // Return out file and plotfile
    strncpy(out_res, out, 100);
    strncpy(plot_res, plot, L_tmpnam);
}


void run_gnu_plot(plot_func_t func, ptr_t func_data, nullary_t cont, ptr_t cont_data)
{
    char out[100], plot[L_tmpnam];
    char cmd[80];
    int res;

    generate_plot_file(func, func_data, out, plot);

    // Remove old file
    sprintf(cmd, "rm -f %s.ps", out);
    res = system(cmd);

    // Run gnuplot
    sprintf(cmd, "gnuplot %s", plot);
    res = system(cmd);

    // Convert to pdf
    inform(string_dappend(string("Converting "), string_dappend(string(out), string(".pdf"))));
    sprintf(cmd, "ps2pdf %1$s.ps %1$s.pdf", out);
    res = system(cmd);

    // Open
    inform(string_dappend(string("Opening "), string_dappend(string(out), string(".pdf"))));
    sprintf(cmd, "open %s.pdf", out);
    res = system(cmd);
}
