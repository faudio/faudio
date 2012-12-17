
#include <doremir/string.h>

struct _doremir_string_t
{               
    size_t  size;
    int16_t *data; // UTF-16
};

