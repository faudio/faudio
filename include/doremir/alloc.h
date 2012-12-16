
#ifndef _DOREMIR_ALLOC
#define _DOREMIR_ALLOC

#define doremir_new(T) malloc(sizeof(struct _doremir_##T##_t))
#define doremir_delete(x) free(x)

#endif // _DOREMIR_ALLOC
