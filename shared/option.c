
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>

#define opt_t fa_option_t
#define fa_push_back_list(xs,x) xs = fa_list_dappend(xs, fa_list_single(x))

pair_t maybe_parse(int optc, opt_t optv[], const char *short_name, const char *long_name, const char *value)
{
    for (int i = 0; i < optc; i++) {
        opt_t option = optv[i];
        // printf("? %s %s\n", short_name, option.short_name);
        // printf("? %s %s\n", long_name, option.long_name);

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

pair_t fa_option_parse(int optc, fa_option_t optv[1], int argc, char *argv[])
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

void fa_option_show(int optc, opt_t optv[], char *header)
{
    printf("%s\n", header);
    printf("Options:\n");

    for (int i = 0; i < optc; ++i) {
        printf("  -%-8s --%-20s %s\n", optv[i].short_name, optv[i].long_name, optv[i].description);
    }

    printf("\n");
}



ptr_t fa_option_integral(char *x)
{
    int r;
    sscanf(x, "%i", &r);
    return i32(r);
}
ptr_t fa_option_floating(char *x)
{
    int r;
    sscanf(x, "%f", &r);
    return f32(r);
}
ptr_t fa_option_string(char *x)
{
    return string(x);
}
ptr_t fa_option_failure(char *x)
{
    return NULL;
}


