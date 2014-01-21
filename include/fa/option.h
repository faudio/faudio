
#ifndef _FA_OPTION
#define _FA_OPTION

#include <fa.h>
#include <fa/std.h>
#include <fa/string.h>
#include <fa/pair.h>

/** @addtogroup FaOption

    Basic options parser.
 
    @defgroup Fa Fa
    @{
    @defgroup FaOption Option
    @{
    */


typedef struct {
            char * short_name;
            char * long_name;
            char * description;
            fa_ptr_t (* parser)(char *);
        } fa_option_t;


fa_ptr_t fa_option_parse(int, fa_option_t [1], int, char * *);


void fa_option_show(int, fa_option_t [1], char *);


fa_ptr_t fa_option_parse_int(char *);


fa_ptr_t fa_option_parse_float(char *);


fa_ptr_t fa_option_parse_double(char *);


fa_ptr_t fa_option_parse_string(char *);


fa_ptr_t fa_option_parse_fail(char *);

/** @}
    @}
    */

#endif // _FA_OPTION

