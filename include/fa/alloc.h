
#ifndef _FA_ALLOC
#define _FA_ALLOC

void* fa_malloc (size_t size);
void* fa_realloc (void* ptr, size_t size);
void fa_free (void* ptr);

#define fa_new(T) fa_malloc(sizeof(struct _fa_##T##_t))
#define fa_new_struct(T) fa_malloc(sizeof(struct T))
#define fa_delete(x) fa_free(x)

#endif // _FA_ALLOC
