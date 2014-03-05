
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
        written += sprintf(&cmd[written], "%s ", argv[i]);
    }
    system(cmd);
    // printf("\n|%s|\n", cmd);
}
