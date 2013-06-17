
#ifndef _FAE_APPLY
#define _FAE_APPLY

typedef void *(* apply_func_0)();
typedef void *(* apply_func_1)(void *);
typedef void *(* apply_func_2)(void *,
                               void *);
typedef void *(* apply_func_3)(void *,
                               void *,
                               void *);
typedef void *(* apply_func_4)(void *,
                               void *,
                               void *,
                               void *);

inline static void *apply0(void *f)
{
  apply_func_0 g = f;
  return g();
}

inline static void *apply1(void *f, void *a)
{
  apply_func_1 g = f;
  return g(a);
}

inline static void *apply2(void *f, void *a, void *b)
{
  apply_func_2 g = f;
  return g(a, b);
}

inline static void *apply3(void *f, void *a, void *b, void *c)
{
  apply_func_3 g = f;
  return g(a, b, c);
}

inline void* apply4(void* f, void* a, void* b, void* c, void* d)
{
  apply_func_4 g = f;
  return g(a, b, c, d);
}

#endif // _FAE_APPLY
