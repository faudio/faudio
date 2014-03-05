
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>
#include "common.h"

int main(int argc, char const *argv[])
{
    char cmd[200];
    int written = 0;

    written += sprintf(&cmd[written], "faudio-");

    for (int i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], "help"))
            written += sprintf(&cmd[written], "%s ", "version");
        else if (0 == strcmp(argv[i], "-h"))
            written += sprintf(&cmd[written], "%s ", "version");
        else if (0 == strcmp(argv[i], "--help"))
            written += sprintf(&cmd[written], "%s ", "version");
        else if (0 == strcmp(argv[i], "-v"))
            written += sprintf(&cmd[written], "%s ", "version");
        else if (0 == strcmp(argv[i], "--version"))
            written += sprintf(&cmd[written], "%s ", "version");
        else
            written += sprintf(&cmd[written], "%s ", argv[i]);
    }
    system(cmd);
    // printf("\n|%s|\n", cmd);
}
