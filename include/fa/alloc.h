
#ifndef _FA_ALLOC
#define _FA_ALLOC

#define fa_new(T) malloc(sizeof(struct _fa_##T##_t))
#define fa_new_struct(T) malloc(sizeof(struct T))
#define fa_delete(x) free(x)

#endif // _FA_ALLOC
