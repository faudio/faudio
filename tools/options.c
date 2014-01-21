
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>


#define fa_unpair(P,A,B) \
    fa_let(__p, P) \
    fa_let(A,fa_pair_first(__p)) \
    fa_let(B,fa_pair_second(__p))

fa_option_t options[] = {
    { "h", "help",            "Show help info",             fa_option_parse_string },
    { "x", "value-with-def",  "Show help info",             fa_option_parse_int    },
    { "n", "number-of-cores", "Number of cores\n                                   "
                              "Very interesting parameter", fa_option_parse_int    }
};

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    fa_option_show(sizeof(options) / sizeof(fa_option_t), options,
                 "Usage: fa_options\n"
                 "       fa_options [FILES]\n"
                );
    pair_t res = fa_option_parse(sizeof(options) / sizeof(fa_option_t), options, argc, (char**) argv);
    fa_unpair(res, os, as) {
        fa_print_ln(fa_map_sum(
            // map(string("foo"), i32(7)), 
            fa_string_from_json(string("{\"foo\":7, \"bar\":false}")), 
            
            os));
        fa_print_ln(as);
    }
    fa_fa_terminate();
}


ptr_t fa_option_parse_int(char *x)    { int r; sscanf(x, "%i", &r); return i32(r); }
ptr_t fa_option_parse_float(char *x)  { int r; sscanf(x, "%f", &r); return f32(r); }
ptr_t fa_option_parse_double(char *x) { int r; sscanf(x, "%lf", &r); return f64(r); }
ptr_t fa_option_parse_string(char *x) { return string(x); }
ptr_t fa_option_parse_fail(char *x)   { return NULL; }