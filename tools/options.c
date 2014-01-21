
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
    ptr_t default_value;
    ptr_t (*parser)(char*); // If null, becomes a boolean toggle
} opt_t;


#define fa_push_back_list(xs,x) xs = fa_list_dappend(xs, fa_list_single(x))

pair_t maybe_parse(int options_count, opt_t options[], const char* short_name, const char*long_name, const char*value)
{
    for (int i = 0; i < options_count; i++) {
        opt_t option = options[i];
        printf("? %s %s\n", short_name, option.short_name);
        printf("? %s %s\n", long_name, option.long_name);
        if (
        
            (short_name && !strcmp(short_name, option.short_name)) 
            || 
            (long_name && !strcmp(long_name, option.long_name))
            ){
            if (!option.parser) {
                return option.default_value;
            } else {
                return pair(string(option.long_name), option.parser((char*) value));
            }
        }
    }
    return NULL;
}

ptr_t parse_options(int options_count, opt_t options[], int argc, char const *argv[])
{
    list_t anon_args = empty();
    map_t args = fa_map_empty();

    enum {short_name, long_name, value} elem_type = value, prev_elem_type = value;
    
    for (int i = 1; i <= argc; ++i) {
        prev_elem_type = elem_type;
        if (i < argc) {
            if (argv[i][0] == '-' && argv[i][1] == '-') {
                elem_type = long_name;
            } else
            if (argv[i][0] == '-' && argv[i][1] != '-') {
                elem_type = short_name;
            } else {
                elem_type = value;
            }
        }
        // TODO on last ?
        if (i == argc || elem_type != value) {
            if (prev_elem_type == short_name) {
                string_t name = string((char*) argv[i-1] + 1);
                args = fa_map_dset(name, fa_from_bool(true), args);
            } else if (prev_elem_type == long_name) {
                string_t name = string((char*) argv[i-1] + 2);
                args = fa_map_dset(name, fa_from_bool(true), args);
            }
        } else {
            if (prev_elem_type == short_name) {
                pair_t result = maybe_parse(options_count, options, argv[i-1] + 1, NULL, argv[i]);
                if (result) {           
                    // TODO set?
                    args = fa_map_add_entry(result, args);
                }
            } else if (prev_elem_type == long_name) {
                pair_t result = maybe_parse(options_count, options, NULL, argv[i-2] + 1, argv[i]);
                if (result) {           
                    // TODO set?
                    args = fa_map_add_entry(result, args);
                }
            } else {
                fa_push_back_list(anon_args, string((char*) argv[i]));            
            }
        }        
    }
    return pair(args, anon_args); // FIXME
}

void show_options(int options_count, opt_t options[], char* header)
{
    printf("%s\n", header);
    printf("Options:\n");
    for (int i = 0; i < options_count; ++i)
        printf("  -%-8s --%-20s %s\n", options[i].short_name, options[i].long_name, options[i].description);
    printf("\n");
}


ptr_t parse_int(char* x)
{               
    int r;
    sscanf (x, "%i", &r);
    return i32(r);
}








opt_t options[] = {
    { "h", "help",            "Show help info",  0,      parse_int },
    { "n", "number-of-cores", "Number of cores", 0,      parse_int }
};

int main(int argc, char const *argv[])
{
    fa_fa_initialize();
    show_options(2, options, 
        "Usage: fa_options\n"
        "       fa_options [FILES]\n"
        );
    fa_print_ln(parse_options(sizeof(options)/sizeof(opt_t), options, argc, argv));
    fa_fa_terminate();
}
