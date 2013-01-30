
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/plot.h>
#include <doremir/thread.h>
#include <doremir/util.h>

typedef doremir_plot_function_t plot_func_t;

void run_gnu_plot(plot_func_t func, ptr_t funcData, nullary_t cont, ptr_t contData)
{
    char dat[L_tmpnam];
    char plot[L_tmpnam];
    tmpnam(dat);
    tmpnam(plot);

    inform(string(dat));
    inform(string(plot));

    FILE * datf = fopen(dat, "w+");
    FILE * plotf = fopen(plot, "w+");

    for (int i = 0; i < 10; ++i) {
        fprintf(datf, "%5f   %5f   %5f   %5f   %5f   %5f   \n", 0.1*i, 0.1*i, 0.2, 0.3, 0.4, 0.5);
    }

    fprintf(plotf,
            "set title 'Audio Engine Plot'                                              \n"
            "set xrange [-1:1]                                                          \n"
            "set yrange [-1:1]                                                          \n"
            "set size 1.0, 1.0                                                          \n"
            "set terminal postscript portrait enhanced mono dashed lw 1 'Helvetica' 14  \n" 
            "set output '%2$s'                                                          \n"
            "set zeroaxis                                                               \n"
            "plot '%1$s' using 1:2 with lines title   'Plot 1',   \\\n"
            "     '%1$s' using 1:3 with lines title   'Plot 2',   \\\n"
            "     '%1$s' using 1:4 with lines title   'Plot 3',   \\\n"
            "     '%1$s' using 1:5 with lines title   'Plot 4',   \\\n"
            "     '%1$s' using 1:6 with lines title   'Plot 5'      \n"
            ,
            dat,
            "/Users/hans/plots/foo.ps"
           );

    fflush(datf);
    fflush(plotf);

    char cmd[80];             
    int res;
    
    sprintf(cmd, "rm -f %s", "/Users/hans/plots/foo.ps");
    inform(string(cmd));
    res = system(cmd);
    inform(format_int("%d\n", res));
    
    sprintf(cmd, "gnuplot %s", plot);
    inform(string(cmd));
    res = system(cmd);
    inform(format_int("%d\n", res));

    sprintf(cmd, "open %s", "/Users/hans/plots/foo.ps");
    inform(string(cmd));
    res = system(cmd);
    inform(format_int("%d\n", res));


    sprintf(cmd, "cat %s", dat);
    inform(string(cmd));
    res = system(cmd);
    inform(format_int("%d\n", res));

    // create tmp.dat
    // write data to it

    // create tmp.plot
    // write plot script

    // run gnuplot
    // open resulting file

    // remove files

    exit(-1);
}
