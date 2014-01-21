
#ifndef _FA_OPTION
#define _FA_OPTION

#include <fa.h>
#include <fa/std.h>
#include <fa/string.h>
#include <fa/pair.h>

/** @addtogroup FaOption
 

Basic options parser.
    
Example:

~~~
fa_option_t options[] = {
    { "h", "help",            "Show help info",             fa_option_parse_string },
    { "x", "value-with-def",  "Show help info",             fa_option_parse_int    },
    { "n", "number-of-cores", "Number of cores\n                                   "
                              "Very interesting parameter", fa_option_parse_int    }
};

int main(int argc, char const *argv[])
{
    fa_option_show_all(
        options,
        "Usage: fa_options\n"
        "       fa_options [FILES]\n"
        );
                
    fa_unpair(
        fa_option_parse_all(options, argc, (char**) argv), 
        options,
        arguments
        ) 
    {
        fa_print_ln(options);   // {"foo": 1, "bar": 2}
        fa_print_ln(arguments); // [1,2,3]
    }
}
~~~    
    
 
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


fa_pair_t fa_option_parse(int, fa_option_t [1], int, char * *);


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

