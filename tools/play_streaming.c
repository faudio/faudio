
#include <fa/fa.h>
#include <fa/util.h>

#include <curl/curl.h>

/*
    This program does ...

 */

typedef fa_atomic_ring_buffer_t ring_buffer_t;
#define ring_buffer(size) fa_atomic_ring_buffer_create(size)

static ring_buffer_t BUFFER;

size_t _write( char *ptr, size_t size, size_t nmemb, void *userdata)
{
    // printf("Ptr: %p, Size: %zu, Num elem: %zu\n", ptr, size, nmemb);
    // printf("Received %f seconds\n", ((float)size*(float)nmemb)/44100);
    size_t received_bytes = size * nmemb;
    size_t written_bytes = 0;
    int    attempts      = 0;

    // fa_thread_sleep(50);
    while (written_bytes < received_bytes)
    {
        if (fa_atomic_ring_buffer_write(BUFFER, ptr[written_bytes])) {
            written_bytes++;
        } else {
            if (attempts++ < 100) {
                fa_thread_sleep(5);
                continue;
            } else {
                return -1; // fail
            }
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
    if(curl) {
      curl_easy_setopt(curl, CURLOPT_URL, "http://localhost:2233/test/test.rawMonoDouble");
      // curl_easy_setopt(curl,CURLOPT_URL, "http://bit.ly/1dU18ZG");
      curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, true);
      curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _write);
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, NULL);

      inform(string("Beginning HTTP request"));
      res = curl_easy_perform(curl);
      if(res != CURLE_OK) {
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

int main(int argc, char const *argv[])
{
    fa_fa_set_log_std();
    fa_fa_initialize();
    BUFFER = ring_buffer(8*44100*5);

    signal_t a = fa_signal_output(0, 11, fa_multiply(fa_signal_play_stream(BUFFER), constant(0.8)));
    signal_t b = fa_signal_input(11);
    {
        fa_audio_session_t s = fa_audio_begin_session();
        fa_audio_device_t i  = fa_audio_default_input(s);
        fa_audio_device_t o  = fa_audio_default_output(s);
        fa_audio_set_parameter(string("sample-rate"), f64(44100), s);

        fa_audio_stream_t st = fa_audio_open_stream(i, o, just, list(a, b));
        if (fa_check(st)) {
            fa_error_log(st, NULL);
        }

        request_audio();
        fa_thread_sleep(300 * 1000);

        fa_audio_close_stream(st);
        fa_audio_end_session(s);
    }


    fa_fa_terminate();
}