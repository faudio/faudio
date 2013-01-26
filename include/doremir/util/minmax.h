
#ifndef _DOREMIR_UTIL_MINMAX
#define _DOREMIR_UTIL_MINMAX

#include <stddef.h>

inline static size_t size_max(size_t a, size_t b)
{
  return (a < b) ? a : b;
}
inline static size_t size_min(size_t a, size_t b)
{
  return (a < b) ? b : a;
}

#endif // _DOREMIR_UTIL_MINMAX

