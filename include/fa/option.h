
#ifndef _FA_OPTION
#define _FA_OPTION

#include <fa.h>
#include <fa/std.h>
#include <fa/string.h>
#include <fa/pair.h>

/** @addtogroup FaOption
 

    Provides a basic option parser.
    
    ## Example

    ~~~
    fa_option_t options[] = {
        { "h", "help",            "Show help info",             fa_option_parse_string },
        { "x", "value-with-def",  "Show help info",             fa_option_parse_int    },
        { "n", "number-of-cores", "Number of cores\n                                   "
                                  "Very interesting parameter", fa_option_parse_int    }
    };

    int main(int argc, char const *argv[])
    {
        fa_option_show_all(options, "Usage: fa_options\n"
                                    "       fa_options [FILES]\n");
                
        fa_unpair(fa_option_parse_all(options, argc, (char**) argv), options, arguments) 
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


typedef fa_ptr_t (* fa_option_parser_t)(char *);


typedef struct {
            char * short_name;
            char * long_name;
            char * description;
            fa_option_parser_t parser;
            char * default_value;
        } fa_option_t;

/** Parses integers. 
*/
fa_ptr_t fa_option_integral(char * input);

/** Parses floating-point numbers. 
*/
fa_ptr_t fa_option_floating(char * input);

/** Parses strings. 
*/
fa_ptr_t fa_option_string(char * input);

/** Always fails. 
*/
fa_ptr_t fa_option_failure(char * input);

/** Parse options according to the given specification (see example above).
    
    @returns
        A pair where the first component is a @ref fa_map_t of `(name, value)`
        pairs, and the second component is a list of remaning arguments.    
    
*/
fa_pair_t fa_option_parse_all(fa_option_t * options,
                              int argc,
                              char * * argv);

/** Show options according to the given specification (see example above) on the standard output.
*/
void fa_option_show_all(fa_option_t * options, char * header);

/** Parse options according to the given specification (see example above).
    
    @returns
        A pair where the first component is a @ref fa_map_t of `(name, value)`
        pairs, and the second component is a list of remaning arguments.    
    
*/
fa_pair_t fa_option_parse(int numOptions,
                          fa_option_t * options,
                          int argc,
                          char * * argv);

/** Show options according to the given specification (see example above) on the standard output.
*/
void fa_option_show(int numOptions,
                    fa_option_t * options,
                    char * header);

/** @}
    @}
    */

#endif // _FA_OPTION

