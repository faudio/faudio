
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>

#define opt_t fa_option_t
#define fa_push_back_list(xs,x) xs = fa_list_dappend(xs, fa_list_single(x))

fa_pair_t maybe_parse(int optc, opt_t optv[], const char *short_name, const char *long_name, const char *value)
{
    for (int i = 0; i < optc; i++) {
        opt_t option = optv[i];

        if (
            (short_name && !strcmp(short_name, option.short_name))
            ||
            (long_name && !strcmp(long_name, option.long_name))
        ) {
            fa_ptr_t result  = option.parser((char *) value);

            if (result) {
                return fa_pair_create(fa_string(option.long_name), result);
            } else {
                return NULL;
            }
        }
    }

    return NULL;
}


// Modify a map by adding defaults (if not set)
fa_map_t add_defaults(int optc, opt_t optv[], fa_map_t args)
{
    for (int i = 0; i < optc; i++) {
        opt_t option = optv[i];

        fa_ptr_t result = option.parser(option.default_value);

        if (result) {
            args = fa_map_dadd(fa_string(option.long_name), result, args);
        }
    }

    return args;
}


fa_pair_t fa_option_parse(int optc, fa_option_t optv[1], int argc, char *argv[])
{
    fa_list_t anon_args = fa_list_empty();
    fa_map_t args = fa_map_empty();

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

        if (i == argc || elem_type != value) {
            if (prev_elem_type == short_name) {
                fa_string_t name = fa_string((char *) argv[i - 1] + 1);
                args = fa_map_dset(name, fa_from_bool(true), args);
            } else if (prev_elem_type == long_name) {
                fa_string_t name = fa_string((char *) argv[i - 1] + 2);
                args = fa_map_dset(name, fa_from_bool(true), args);
            }
        } else {
            if (prev_elem_type == short_name) {
                fa_pair_t result = maybe_parse(optc, optv, argv[i - 1] + 1, NULL, argv[i]);

                if (result) {
                    args = fa_map_dadd(fa_pair_first(result), fa_pair_second(result), args);
                }
            } else if (prev_elem_type == long_name) {
                fa_pair_t result = maybe_parse(optc, optv, NULL, argv[i - 1] + 2, argv[i]);

                if (result) {
                    args = fa_map_dadd(fa_pair_first(result), fa_pair_second(result), args);
                }
            } else {
                fa_push_back_list(anon_args, fa_string((char *) argv[i]));
            }
        }
    }

    return fa_pair_create(add_defaults(optc, optv, args), anon_args); // FIXME
}

void fa_option_show(int optc, opt_t optv[], char *header)
{
    printf("%s\n", header);
    printf("Options:\n");

    for (int i = 0; i < optc; ++i) {
        printf("  -%-8s --%-20s %s (default %s)\n",
               optv[i].short_name,
               optv[i].long_name,
               optv[i].description,
               optv[i].default_value);
    }

    printf("\n");
}



fa_ptr_t fa_option_integral(char *x)
{
    int r;
    sscanf(x, "%i", &r);
    return fa_i32(r);
}
fa_ptr_t fa_option_floating(char *x)
{
    double r;
    sscanf(x, "%lf", &r);
    return fa_f64(r);
}
fa_ptr_t fa_option_string(char *x)
{
    return fa_string(x);
}
fa_ptr_t fa_option_native_string(char *x)
{
    #ifdef _WIN32
    return fa_string_from_cp1252(x);
    #else
    return fa_string_from_utf8(x);
    #endif
}
fa_ptr_t fa_option_failure(char *x)
{
    return NULL;
}


