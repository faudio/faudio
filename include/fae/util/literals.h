
#ifndef _FAE_UTIL_LITERALS
#define _FAE_UTIL_LITERALS

#include <fae/list.h>
#include <fae/set.h>
#include <fae/map.h>
#include <fae/util/vararg.h>

fae_list_t fae_list(int count, ...);
fae_set_t fae_set(int count, ...);
fae_map_t fae_map(int count, ...);

#define list0()                             fae_list(0)
#define list1(a)                            fae_list(1,a)
#define list2(a,b)                          fae_list(2,a,b)
#define list3(a,b,c)                        fae_list(3,a,b,c)
#define list4(a,b,c,d)                      fae_list(4,a,b,c,d)
#define list5(a,b,c,d,e)                    fae_list(5,a,b,c,d,e)
#define list(...) VARARG(list, __VA_ARGS__)

#define set0()                              fae_set(0)
#define set1(a)                             fae_set(1,a)
#define set2(a,b)                           fae_set(2,a,b)
#define set3(a,b,c)                         fae_set(3,a,b,c)
#define set4(a,b,c,d)                       fae_set(4,a,b,c,d)
#define set5(a,b,c,d,e)                     fae_set(5,a,b,c,d,e)
#define set(...) VARARG(set, __VA_ARGS__)

#define map0()                              fae_map(0)
#define map1(a)                             fae_map(1,a)
#define map2(a,b)                           fae_map(2,a,b)
#define map3(a,b,c)                         fae_map(3,a,b,c)
#define map4(a,b,c,d)                       fae_map(4,a,b,c,d)
#define map5(a,b,c,d,e)                     fae_map(5,a,b,c,d,e)
// #define map6(a,b,c,d,e,f)                   fae_map(6,a,b,c,d,e,f)
#define map(...) VARARG(map, __VA_ARGS__)

#endif // _FAE_UTIL_LITERALS

