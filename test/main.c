
#include <doremir/audio_engine.h>
// #include <pthread.h>
// #include <sndfile.h>
// #include <portaudio.h>
// #include <portmidi.h>
#include <getopt.h>
#include <iconv.h>

int version[3] = { 2, 0, 0 };


intptr_t printer(intptr_t x)
{
    int n = 0;
    while (n < 100)
    {
        printf("%d\n", n++);
    }
    return 0;
}
void test_thread()
{                                               
    doremir_thread_runnable_t r = { printer, 0 };
    doremir_thread_t t = doremir_thread_create(r);
    doremir_thread_join(t);
}


int main (int argc, char const *argv[])
{
  printf("DoReMIR Audio engine v%d.%d.%d\n", version[0], version[1], version[2]);
  printf("sizeof(intptr_t) = %d\n", (unsigned int) sizeof(intptr_t));
  printf("sizeof(int32_t) = %d\n", (unsigned int) sizeof(int32_t));
  printf("sizeof(uint32_t) = %d\n", (unsigned int) sizeof(uint32_t));
  
  // int c = getopt(argc, (char**) argv, "abc:");
  // iconv_t cd = iconv_open("WCHAR_T", "UTF-8");
  
  test_thread();
  
  return 0;
}