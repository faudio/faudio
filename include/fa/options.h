
#ifndef _FA_OPTIONS
#define _FA_OPTIONS

#include <fa.h>
#include <fa/std.h>
#include <fa/string.h>
#include <fa/pair.h>

/** @addtogroup FaOptions

    Basic options parser.
 
    @defgroup Fa Fa
    @{
    @defgroup FaOptions Options
    @{
    */


typedef struct {
            char * short_name;
            char * long_name;
            char * description;
            fa_ptr_t (* parser)(char *);
        } fa_options_opt_t;


fa_ptr_t fa_options_parse_options(int,
                                  fa_options_opt_t [1],
                                  int,
                                  char * *);


void fa_options_show_options(int, fa_options_opt_t [1], char *);


void fa_options_parse_int(char *);


void fa_options_parse_float(char *);


void fa_options_parse_double(char *);


void fa_options_parse_string(char *);


void fa_options_parse_fail(char *);

/** @}
    @}
    */

#endif // _FA_OPTIONS

