
#ifdef __APPLE__
#include "thread_osx.c"
#endif

#ifdef WIN32
#include "thread_win.c"
#endif