
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/plot.h>
#include <doremir/thread.h>
#include <doremir/system/directory.h>
#include <doremir/util.h>

typedef doremir_plot_function_t plot_func_t;

#define samples_k           500000
#define down_sample_k       10
#define path_max_lenght_k   100
#define tmp_max_length_k    L_tmpnam
#define cmd_max_length_k    100
#define num_plots_k         5
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

void generate_plot_file(plot_func_t func, 
                        ptr_t func_data, 
                        char *out_dir_res, 
                        char *out_res, 
                        char *plot_res
                        )
{
    char plot_path[tmp_max_length_k];   // Plot file (returned)

    char out_dir[path_max_lenght_k];    // Output directory
    char data_path[tmp_max_length_k];   // Data file (coded into plot file)
    char out_path[path_max_lenght_k];   // Output file (coded into plot file and returned)

    tmpnam(data_path);
    tmpnam(plot_path);

    doremir_with_temp(home, doremir_system_directory_home()) {
        snprintf(out_dir,   path_max_lenght_k, "%s/.doremiraudio", unstring(home));
        snprintf(out_path,  path_max_lenght_k, "%s/plot", out_dir);
    }

    // Write data and plot file
    doremir_with(data_file, fopen(data_path, "w+"), fclose(data_file)) {
        doremir_with(plot_file, fopen(plot_path, "w+"), fclose(plot_file)) {

            for (int sample = 0; sample < samples_k; ++sample) {
                double x = ((double) sample) / ((double) samples_k) * 2 - 1;
                double ys[num_plots_k];

                for (int index = 0; index < num_plots_k; ++index) {
                    ys[index] = func(func_data, index, 0, x);
                }

                fprintf(data_file, "%f ", x);

                for (int i = 0; i < num_plots_k; ++i) {
                    fprintf(data_file, "%f ", ys[0]);
                }

                fprintf(data_file, "\n");
            }
            fprintf(plot_file, plot_format_k, data_path, out_path, down_sample_k);
        }
    }

    // Return out_path file and plotfile
    strncpy(out_dir_res,    out_dir,   path_max_lenght_k);
    strncpy(out_res,        out_path,  path_max_lenght_k);
    strncpy(plot_res,       plot_path, L_tmpnam);
}


void run_gnu_plot(plot_func_t func, ptr_t func_data, nullary_t cont, ptr_t cont_data)
{
    char out[path_max_lenght_k], out_dir[path_max_lenght_k], plot[tmp_max_length_k], cmd[cmd_max_length_k];
    int res;


    generate_plot_file(func, func_data, out_dir, out, plot);

    inform(string_dappend(string("Creating "), 
           string_dappend(string(out), string(".ps"))));

    doremir_system_directory_create(string(out_dir));

    // Remove old file

    sprintf(cmd, "rm -f %s.ps", out);
    res = system(cmd);

    // Run gnuplot

    sprintf(cmd, "gnuplot %s", plot);
    res = system(cmd);

    // Convert to pdf

    inform(string_dappend(string("Converting "), 
                         string_dappend(string(out), 
                                        string(".pdf"))));

    sprintf(cmd, "ps2pdf %1$s.ps %1$s.pdf", out);
    res = system(cmd);

    // Open

    inform(string_dappend(string("Opening "), 
                          string_dappend(string(out), 
                                         string(".pdf"))));

    sprintf(cmd, "open %s.pdf", out);
    res = system(cmd);
}
