
#include <fa/fa.h>
#include <fa/util.h>

/*
    This program does ...

 */

void helper_function()
{
    fa_print_ln(string("This is fa_template!"));
}


typedef struct opt {
    char *short_name, *long_name, *description;
    ptr_t (*parser)(char *);
} opt_t;


#define fa_push_back_list(xs,x) xs = fa_list_dappend(xs, fa_list_single(x))

pair_t maybe_parse(int optc, opt_t optv[], const char *short_name, const char *long_name, const char *value)
{
    for (int i = 0; i < optc; i++) {
        opt_t option = optv[i];
        printf("? %s %s\n", short_name, option.short_name);
        printf("? %s %s\n", long_name, option.long_name);

        if (
            (short_name && !strcmp(short_name, option.short_name))
            ||
            (long_name && !strcmp(long_name, option.long_name))
        ) {
            ptr_t result = option.parser((char *) value);
            if (result) {
                return pair(string(option.long_name), result);
            } else {
                return NULL;
            }
        }
    }

    return NULL;
}

ptr_t parse_options(int optc, opt_t optv[], int argc, char const *argv[])
{
    list_t anon_args = empty();
    map_t args = fa_map_empty();

    enum {short_name, long_name, value} elem_type = value, prev_elem_type = value;

    for (int i = 1; i <= argc; ++i) {
        prev_elem_type = elem_type;

        if (i < argc) {
            if (argv[i][0] == '-' && argv[i][1] == '-') {
                elem_type = long_name;
            } else if (argv[i][0] == '-' && argv[i][1] != '-') {
                elem_type = short_name;
            } else {
                elem_type = value;
            }
        }

        // TODO on last ?
        if (i == argc || elem_type != value) {
            if (prev_elem_type == short_name) {
                string_t name = string((char *) argv[i - 1] + 1);
                args = fa_map_dset(name, fa_from_bool(true), args);
            } else if (prev_elem_type == long_name) {
                string_t name = string((char *) argv[i - 1] + 2);
                args = fa_map_dset(name, fa_from_bool(true), args);
            }
        } else {
            if (prev_elem_type == short_name) {
                pair_t result = maybe_parse(optc, optv, argv[i - 1] + 1, NULL, argv[i]);
                if (result) {
                    args = fa_map_add_entry(result, args);
                }
            } else if (prev_elem_type == long_name) {
                pair_t result = maybe_parse(optc, optv, NULL, argv[i - 2] + 1, argv[i]);
                if (result) {
                    args = fa_map_add_entry(result, args);
                }
            } else {
                fa_push_back_list(anon_args, string((char *) argv[i]));
            }
        }
    }

    return pair(args, anon_args); // FIXME
}

void show_options(int optc, opt_t optv[], char *header)
{
    printf("%s\n", header);
    printf("Options:\n");

    for (int i = 0; i < optc; ++i) {
        printf("  -%-8s --%-20s %s\n", optv[i].short_name, optv[i].long_name, optv[i].description);
    }

    printf("\n");
}





ptr_t parse_options(int optc, opt_t optv[0], int argc, char const *argv[]);

void  show_options(int optc, opt_t optv[0], char *header);
ptr_t parse_int(char *x)    { int r; sscanf(x, "%i", &r); return i32(r); }
ptr_t parse_float(char *x)  { int r; sscanf(x, "%f", &r); return f32(r); }
ptr_t parse_double(char *x) { int r; sscanf(x, "%lf", &r); return f64(r); }
ptr_t parse_string(char *x) { return string(x); }
ptr_t parse_fail(char *x)   { return NULL; }






#define fa_unpair(P,A,B) \
    fa_let(__p, P) \
    fa_let(A,fa_pair_first(__p)) \
    fa_let(B,fa_pair_second(__p))

opt_t options[] = {
    { "h", "help",            "Show help info",             parse_string },
    { "x", "value-with-def",  "Show help info",             parse_int    },
    { "n", "number-of-cores", "Number of cores\n                                   "
                              "Very interesting parameter", parse_int    }
};

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    show_options(sizeof(options) / sizeof(opt_t), options,
                 "Usage: fa_options\n"
                 "       fa_options [FILES]\n"
                );
    pair_t res = parse_options(sizeof(options) / sizeof(opt_t), options, argc, argv);
    fa_unpair(res, os, as) {
        fa_print_ln(fa_map_sum(
            // map(string("foo"), i32(7)), 
            fa_string_from_json(string("{\"foo\":7, \"bar\":false}")), 
            
            os));
        fa_print_ln(as);
    }
    fa_fa_terminate();
}
