
/*
    faudio
    
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/plot.h>
#include <fa/thread.h>
#include <fa/system.h>
#include <fa/util.h>

typedef fa_plot_function_t plot_func_t;

#define fa_iter(var, begin, end) \
    for (int var = begin; var < end; ++var)

#define kSamples        500000

#define kDownSapling    10
#define kPathSize       100
#define kCmdSize        100
#define kTmpSize        L_tmpnam

#define kPlots          5
#define kPlotFormat \
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

void generate_plot_file(plot_func_t func,
                        ptr_t func_data,
                        char *out_dir_res,
                        char *out_res,
                        char *plot_res
                       )
{
    char plot_path[kTmpSize];   // Plot file (returned)

    char out_dir[kPathSize];    // Output directory
    char data_path[kTmpSize];   // Data file (coded into plot file)
    char out_path[kPathSize];   // Output file (coded into plot file and returned)

    tmpnam(data_path);
    tmpnam(plot_path);

    fa_with_temp(home, fa_system_directory_home()) {
        snprintf(out_dir,   kPathSize, "%s/.faaudio", unstring(home));
        snprintf(out_path,  kPathSize, "%s/plot", out_dir);
    }

    // Write data and plot file

    fa_with(data_file, fopen(data_path, "w+"), fclose(data_file)) {

        fa_iter(sample, 0, kSamples) {
            // for (int sample = 0; sample < kSamples; ++sample) {

            double x = ((double) sample) / ((double) kSamples) * 2 - 1;
            double ys[kPlots];

            for (int index = 0; index < kPlots; ++index) {
                ys[index] = func(func_data, index, 0, x);
            }

            fprintf(data_file, "%f ", x);

            for (int i = 0; i < kPlots; ++i) {
                fprintf(data_file, "%f ", ys[i]);
            }

            fprintf(data_file, "\n");
        }

    }
    fa_with(plot_file, fopen(plot_path, "w+"), fclose(plot_file)) {
        fprintf(plot_file, kPlotFormat, data_path, out_path, kDownSapling);
    }

    // Return out_path file and plotfile
    strncpy(out_dir_res, out_dir,   kPathSize);
    strncpy(out_res,     out_path,  kPathSize);
    strncpy(plot_res,    plot_path, kTmpSize);
}


void run_gnu_plot(plot_func_t func, ptr_t func_data, nullary_t cont, ptr_t cont_data)
{
    char out_path[kPathSize];
    char out_dir[kPathSize];
    char plot_path[kTmpSize];
    char cmd[kCmdSize];
    int res;


    generate_plot_file(func, func_data, out_dir, out_path, plot_path);

    inform(string_dappend(string("Creating "),
                          string_dappend(string(out_path),
                                         string(".ps"))));

    // Assure output directory exists
    fa_system_directory_create(string(out_dir));

    // Remove old file
    sprintf(cmd, "rm -f %s.ps", out_path);
    res = system(cmd);

    // Run gnuplot, yielding PostScript
    sprintf(cmd, "gnuplot %s", plot_path);
    res = system(cmd);

    // Convert to PostScript to PDF
    inform(string_dappend(string("Converting "),
                          string_dappend(string(out_path),
                                         string(".pdf"))));

    sprintf(cmd, "ps2pdf %1$s.ps %1$s.pdf", out_path);
    res = system(cmd);

    // Open
    inform(string_dappend(string("Opening "),
                          string_dappend(string(out_path),
                                         string(".pdf"))));

    sprintf(cmd, "open %s.pdf", out_path);
    res = system(cmd);
}
