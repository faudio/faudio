
#include <doremir/audio_engine.h>
// #include <pthread.h>
// #include <sndfile.h>
// #include <portaudio.h>
// #include <portmidi.h>

int version[3] = { 2, 0, 0 };

int main (int argc, char const *argv[])
{
  printf("DoReMIR Audio engine v%d.%d.%d\n", version[0], version[1], version[2]);
  printf("sizeof(intptr_t) = %d\n", sizeof(intptr_t));
  printf("sizeof(int32_t) = %d\n", sizeof(int32_t));
  printf("sizeof(uint32_t) = %d\n", sizeof(uint32_t));

  
  return 0;
}