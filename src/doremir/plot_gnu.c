
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/plot.h>
#include <doremir/thread.h>
#include <doremir/util.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

typedef doremir_plot_function_t plot_func_t;

void run_gnu_plot(plot_func_t func, ptr_t func_data, nullary_t cont, ptr_t cont_data)
{
    struct passwd *passwdEnt = getpwuid(getuid());
    char *home = passwdEnt->pw_dir;

    char dat[L_tmpnam], plot[L_tmpnam], out_dir[100], out[100];

    tmpnam(dat);
    tmpnam(plot);
    sprintf(out_dir, "%s/.doremiraudio", home);
    sprintf(out, "%s/plot", out_dir);

    inform(string_dappend(string("Creating "), string_dappend(string(out), string(".ps"))));

    mkdir(out_dir, (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH));

    FILE *datf = fopen(dat, "w+");
    FILE *plotf = fopen(plot, "w+");

    int samples     = 500000;
    int down_sample = 10;
    // int samples     = 10;
    // int down_sample = 1;

    for (int sample = 0; sample < samples; ++sample) {

        double x = ((double) sample) / ((double) samples) * 2 - 1;
        double ys[5];

        for (int index = 0; index < 5; ++index) {
            ys[index] = func(func_data, index, 0, x);
        }

        fprintf(datf, "%5f   %5f   %5f   %5f   %5f   %5f   \n", x, ys[0], ys[1], ys[2], ys[3], ys[4]);
    }

    fprintf(plotf,
            "set title 'Audio Engine Plot'                                              \n"
            "set xrange [-1:1]                                                          \n"
            "set yrange [-1:1]                                                          \n"
            "set size 1.0, 1.0                                                          \n"
            "set terminal postscript landscape enhanced mono lw 1 'Helvetica' 14        \n"
            "set output '%2$s.ps'                                                       \n"
            "set zeroaxis                                                               \n"
            "plot '%1$s' using 1:2 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 1',   \\\n"
            "     '%1$s' using 1:3 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 2',   \\\n"
            "     '%1$s' using 1:4 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 3',   \\\n"
            "     '%1$s' using 1:5 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 4',   \\\n"
            "     '%1$s' using 1:6 every %3$d with lines lc rgbcolor '#a0a0ff' title 'Plot 5'      \n"
            ""
            ,
            dat,
            out,
            down_sample
           );

    fflush(datf);
    fflush(plotf);

    char cmd[80];
    int res;

    sprintf(cmd, "rm -f %s.ps", out);
    res = system(cmd);

    sprintf(cmd, "gnuplot %s", plot);
    res = system(cmd);

    // inform(string_dappend(string("Opening "), string_dappend(string(out), string(".ps"))));
    // sprintf(cmd, "open %s.ps", out);
    // res = system(cmd);

    inform(string_dappend(string("Converting "), string_dappend(string(out), string(".pdf"))));
    sprintf(cmd, "ps2pdf %1$s.ps %1$s.pdf", out);
    res = system(cmd);

    inform(string_dappend(string("Opening "), string_dappend(string(out), string(".pdf"))));
    sprintf(cmd, "open %s.pdf", out);
    res = system(cmd);
}
