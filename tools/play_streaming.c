
#include <fa/fa.h>
#include <fa/util.h>
#include <curl/curl.h>
#include "common.h"

/*
    This program does ...

 */

typedef fa_atomic_ring_buffer_t ring_buffer_t;
#define ring_buffer(size) fa_atomic_ring_buffer_create(size)

#define BUFFER_SIZE_MILLIS 2000

static ring_buffer_t BUFFER;

size_t _write(char *ptr, size_t size, size_t nmemb, void *userdata)
{
    // printf("Ptr: %p, Size: %zu, Num elem: %zu\n", ptr, size, nmemb);
    // printf("Received %f seconds\n", ((float)size*(float)nmemb)/44100);
    size_t received_bytes = size * nmemb;
    size_t written_bytes = 0;

    // fa_thread_sleep(50);
    while (written_bytes < received_bytes) {
        if (fa_atomic_ring_buffer_write(BUFFER, ptr[written_bytes])) {
            written_bytes++;
        } else {
            // Spin until buffer can accept input
            fa_thread_sleep(1);
            continue;
        }
    }

    // return written;
    return written_bytes;
}

void request_audio()
{
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();

    if (curl) {
        // curl_easy_setopt(curl, CURLOPT_URL, "http://localhost:2233/test/test.raw");
        curl_easy_setopt(curl, CURLOPT_URL, "http://bit.ly/1dU18ZG");
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, true);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _write);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, NULL);

        inform(string("Beginning HTTP request"));
        res = curl_easy_perform(curl);

        if (res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }

        inform(string("Finished HTTP request"));
        curl_easy_cleanup(curl);
    }
}

list_t just(ptr_t x, list_t xs)
{
    return x;
}

// TODO move
#define fa_with_session_(V) fa_with_temp(V, fa_audio_begin_session())
#define fa_with_default_devices(I,O,S) \
    fa_let(I, fa_audio_default_input(S)) \
    fa_let(O, fa_audio_default_output(S))
#define fa_audio_open_output(I,O,SIG) \
    fa_audio_open_stream(I,O,just,SIG)

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_with_faudio() {

        BUFFER = ring_buffer((8L * 44100L * BUFFER_SIZE_MILLIS) / 1000L);

        // signal_t left = fa_multiply(fa_signal_play_stream(BUFFER), constant(0.8));
        signal_t left = fa_multiply(fa_signal_random(), constant(0.0));
        signal_t right = fa_multiply(fa_signal_random(), constant(0.0));

        fa_with_session_(session) {
            fa_with_default_devices(input, output, session) {
                fa_audio_set_parameter(string("sample-rate"), f64(44100), session);
                fa_audio_stream_t stream;

                if (fa_check(stream = fa_audio_open_output(input, output, list(left, right)))) {
                    fa_error_log(stream, NULL);
                } else {
                    request_audio();
                    // TODO some way to delay or toggle DSP thread reading from stream (give it a rate argument?)
                    fa_thread_sleep(300 * 1000);
                }
            }
        }
    }
}
