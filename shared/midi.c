
#ifdef _WIN32
#include "midi/portmidi.c"
#else
#include "../platform/macosx/midi.c"
// #include "midi/portmidi.c"
#endif

