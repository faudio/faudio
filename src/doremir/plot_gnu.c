
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
    struct passwd * passwdEnt = getpwuid(getuid());
    char * home = passwdEnt->pw_dir;

    char dat[L_tmpnam];
    char plot[L_tmpnam];
    char out_dir[100];
    char out[100];
    tmpnam(dat);
    tmpnam(plot);

    sprintf(out_dir, "%s/.doremiraudio", home);
    sprintf(out, "%s/plot.ps", out_dir);

    mkdir(out_dir, (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH));

    warn(string(dat));
    warn(string(plot));
    warn(string(out_dir));
    warn(string(out));

    FILE * datf = fopen(dat, "w+");
    FILE * plotf = fopen(plot, "w+");

    int samples = 44100*50;
    
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
            "set output '%2$s'                                                          \n"
            "set zeroaxis                                                               \n"
            "plot '%1$s' using 1:2 every 100 with lines lc rgbcolor '#a0a0b0' title 'Plot 1',   \\\n"
            "     '%1$s' using 1:3 every 100 with lines lc rgbcolor '#a0a0b0' title 'Plot 2',   \\\n"
            "     '%1$s' using 1:4 every 100 with lines lc rgbcolor '#a0a0b0' title 'Plot 3',   \\\n"
            "     '%1$s' using 1:5 every 100 with lines lc rgbcolor '#a0a0b0' title 'Plot 4',   \\\n"
            "     '%1$s' using 1:6 every 100 with lines lc rgbcolor '#a0a0b0' title 'Plot 5'      \n"          
            ""
            ,
            dat,
            out
           );

    fflush(datf);
    fflush(plotf);

    char cmd[80];
    int res;

    sprintf(cmd, "rm -f %s", out);
    inform(string(cmd));
    res = system(cmd);
    inform(format_int("%d\n", res));

    sprintf(cmd, "gnuplot %s", plot);
    inform(string(cmd));
    res = system(cmd);
    inform(format_int("%d\n", res));

    sprintf(cmd, "open %s", out);
    inform(string(cmd));
    res = system(cmd);
    inform(format_int("%d\n", res));

    exit(-1);
}
