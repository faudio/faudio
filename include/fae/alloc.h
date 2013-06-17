
#ifndef _FAE_ALLOC
#define _FAE_ALLOC

#define fae_new(T) malloc(sizeof(struct _fae_##T##_t))
#define fae_new_struct(T) malloc(sizeof(struct T))
#define fae_delete(x) free(x)

#endif // _FAE_ALLOC
