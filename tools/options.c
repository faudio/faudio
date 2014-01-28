
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/option.h>
#include <fa/io.h>


// TODO move
#define fa_option_show_all(A,S) fa_option_show(fa_sizeof_array(A),A,S)
#define fa_option_parse_all(A,AC,AV) fa_option_parse(fa_sizeof_array(A), A, AC, AV)

fa_option_t options[] = {
    { "a", "help",            "Show help info",             fa_option_string },
    { "e", "help",            "Show help info",             fa_option_string },
    { "h", "help",            "Show help info",             fa_option_string },
    { "x", "value-with-def",  "Show help info",             fa_option_integral    },
    {
        "n", "number-of-cores", "Number of cores\n                                   "
        "Very interesting parameter", fa_option_integral
    }
};



static buffer_t store[5];
static int store_count = 0;
// static int pushes = 0;
// static int pulls = 0;

void push_counter(fa_ptr_t x, fa_buffer_t buffer)
{
    // printf("Number of pushes: %d\n", ++pushes);
    store[store_count++] = fa_copy(buffer);
    // printf("Stored at index: %d\n", store_count-1);
}
void pull_counter(fa_ptr_t x, fa_io_callback_t cb, ptr_t data)
{
    // printf("Number of pulls: %d\n", ++pulls);
    if (store_count == 4) {
        while (store_count > 0) {
            // printf("Fetching index: %d\n", store_count-1);
            cb(data, store[--store_count]);
        }
    }
}


int main(int argc, char const *argv[])
{
    fa_initialize();
    printf("Size is %zu\n", fa_sizeof_array(options));


    fa_option_show_all(options,
                       "Usage: fa_options\n"
                       "       fa_options [FILES]\n"
                      );

    fa_unpair(
        fa_option_parse_all(options, argc, (char **) argv),
        os, as
    ) {
        fa_print_ln(fa_map_sum(
                        // apply(string("foo"), i32(7)),
                        fa_string_from_json(string("{\"foo\":7, \"bar\":false}")),
                        os));
        fa_print_ln(as);

        fa_io_run(
            fa_io_apply(
                fa_io_standard_in(), 
                fa_io_create_simple_filter(push_counter, pull_counter, NULL)
                ), 
                fa_io_standard_out());
        // fa_io_run(
        //     fa_io_apply(
        //         // fa_io_standard_in(),
        //         fa_io_read_file(string("test/test.wav")),
        //         fa_io_split(fa_io_write_file(string("foo2.wav")))
        //     ),
        //     fa_io_coapply(
        //         fa_io_compose(
        //             // fa_io_split(fa_io_write_file(string("log.txt"))),
        //             // fa_io_split(fa_io_write_file(string("log2.txt")))
        //             fa_io_split(fa_io_write_file(string("foo.wav"))),
        //             fa_io_identity()
        //         ),
        //         fa_io_standard_out()));

    }
    fa_terminate();
}
