
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

/*
    This program prints the version of faudio.
 */

int main(int argc, char const *argv[])
{
    fa_with_faudio() {


        // TODO split version/help
        printf("faudio-%s\n", unstring(
                   string_dappend(fa_version_string(),
                                  string_dappend(string(""),
#ifdef FAUDIO_DEBUG
                                                 string(" (debug build)")))
#else
                                                 string(" (release build)")))
#endif

               ));

        printf("\n");

        printf("Usage: faudio [--version] [--help]\n");
        printf("              <command> [<args>]\n");

        printf("\n");

        printf("Commands:\n");
        printf("    sine   [-d] [-f] [-a] [-r] [-l] [-v]\n");
        printf("    stereo [-d] [-f] [-a] [-r] [-l] [-v]\n");
        printf("    level\n");
        printf("    record\n");
        printf("    play\n");

    }

}
