
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/io.h>
#include <fa/buffer.h>
#include "common.h"

// Define high-resolution timer, from
// http://stackoverflow.com/questions/2349776/how-can-i-benchmark-c-code-easily

#ifdef _WIN32
#include <windows.h>
double get_time()
{
    LARGE_INTEGER t, f;
    QueryPerformanceCounter(&t);
    QueryPerformanceFrequency(&f);
    return (double)t.QuadPart/(double)f.QuadPart;
}

#else
#include <sys/time.h>
#include <sys/resource.h>
double get_time()
{
    struct timeval t;
    struct timezone tzp;
    gettimeofday(&t, &tzp);
    return t.tv_sec + t.tv_usec*1e-6;
}
#endif


fa_buffer_t test_file_to_buffer(fa_string_t path) {
    fa_inform(fa_string("=== FILE TO BUFFER ==="));
    fa_inform(fa_string("    Reading file into buffer"));
    fa_io_source_t source = fa_io_read_file(path);
    double time1 = get_time();
    fa_buffer_t buffer = fa_io_pull_to_buffer(source);
    double time2 = get_time();
    fa_inform(fa_format_floating("    in %.2f ms", 1000.0 * (time2 - time1)));
    fa_destroy(source);
    return buffer;
}

fa_buffer_t test_file_to_ogg(fa_string_t path) {
    fa_inform(fa_string("=== FILE TO OGG ==="));
    fa_inform(fa_string("    Reading file directly"));
    fa_io_source_t raw_source = fa_io_read_file(path);
    fa_io_source_t source = fa_io_apply(raw_source, fa_io_create_ogg_encoder(44100, 1));
    double time1 = get_time();
    fa_buffer_t ogg_buffer = fa_io_pull_to_buffer(source);
    double time2 = get_time();
    size_t ogg_size = fa_buffer_size(ogg_buffer);
    fa_inform(fa_format_integral("    -> converted to %zu bytes of ogg", ogg_size));
    fa_inform(fa_format_floating("    in %.2f ms", 1000.0 * (time2 - time1)));
    fa_destroy(source);
    fa_slog_info("    ", ogg_buffer);
    return ogg_buffer;
}

fa_buffer_t test_buffer_to_ogg(fa_string_t path) {
    fa_inform(fa_string("=== BUFFER TO OGG ==="));
    fa_inform(fa_string("    Reading file to buffer"));
    fa_buffer_t source_buffer = fa_buffer_read_raw(path);
    if (!source_buffer) {
        fa_slog_error("    Error: could not read file to buffer: ", path);
    }
    fa_inform(fa_format_integral("    -> %zu bytes of audio data in buffer", fa_buffer_size(source_buffer)));
    fa_io_source_t raw_source = fa_io_from_buffer(source_buffer);
    fa_io_source_t source = fa_io_apply(raw_source, fa_io_create_ogg_encoder(44100, 1));
    double time1 = get_time();
    fa_buffer_t ogg_buffer = fa_io_pull_to_buffer(source);
    double time2 = get_time();
    size_t ogg_size = fa_buffer_size(ogg_buffer);
    fa_inform(fa_format_integral("    -> converted to %zu bytes of ogg", ogg_size));
    fa_inform(fa_format_floating("    in %.2f ms", 1000.0 * (time2 - time1)));
    fa_destroy(source_buffer);
    fa_destroy(source);
    fa_slog_info("    ", ogg_buffer);
    return ogg_buffer;
}

void test_buffer_to_file(fa_buffer_t buffer, fa_string_t path) {
    fa_inform(fa_string("=== BUFFER TO FILE ==="));
    fa_inform(fa_format_integral("    -> writing %zu bytes to file", fa_buffer_size(buffer)));
    fa_io_source_t source = fa_io_from_buffer(buffer);
    fa_io_sink_t sink = fa_io_write_file(path);
    double time1 = get_time();
    fa_io_run(source, sink);
    fa_destroy(source);
    fa_destroy(sink);
    double time2 = get_time();
    fa_inform(fa_format_floating("    in %.2f ms", 1000.0 * (time2 - time1)));
}

int main(int argc, char const *argv[])
{
    fa_set_log_std();
    if (argc < 2 || argc > 3) {
        printf("Usage: faudio-io_simple_filter <infile> [<outfile>]\n");
        exit(1);
    }
    fa_string_t infile  = fa_string_from_utf8(argv[1]);
    fa_string_t outfile = argc >= 3 ? fa_string_from_utf8(argv[2]) : NULL;
    
    fa_slog_info("infile: ", infile);
    //fa_slog_info("outfile", outfile);
    
    if (outfile) {
        fa_slog_warning("outfile not implemented");
    }
    
    fa_buffer_t raw1 = test_file_to_buffer(fa_copy(infile));
    fa_buffer_t ogg1 = test_file_to_ogg(fa_copy(infile));
    fa_buffer_t ogg2 = test_buffer_to_ogg(infile);
    
    fa_buffer_write_raw(fa_string("io-raw1"), raw1);
    fa_buffer_write_raw(fa_string("io-ogg1.ogg"), ogg1);
    fa_buffer_write_raw(fa_string("io-ogg2.ogg"), ogg2);
    
    test_buffer_to_file(raw1, fa_string("io-raw2"));
    
    fa_destroy(raw1);
    fa_destroy(ogg1);
    fa_destroy(ogg2);
        
    // fa_with_faudio() {
    //     fa_io_filter_t filter = fa_io_create_test_filter();
    //     fa_io_sink_t sink = (fa_io_sink_t) filter;
    //
    //
    //     for (int i = 0; i < 3; ++i) {
    //         fa_buffer_t buf = fa_buffer_create(3);
    //         fa_buffer_set(buf, 0, 1);
    //         fa_buffer_set(buf, 0, 2);
    //         fa_buffer_set(buf, 0, 3);
    //         fa_io_push(sink, buf);
    //     }
    //
    //     // fa_io_run(
    //     //     fa_io_standard_in(),
    //     //     fa_io_coapply(fa_io_split(sink), fa_io_standard_out())
    //     //         );
    // }
}
