
#ifndef _DOREMIR_UTIL_LITERALS
#define _DOREMIR_UTIL_LITERALS

#include <doremir/list.h>
#include <doremir/set.h>
#include <doremir/map.h>
#include <doremir/util/vararg.h>

doremir_list_t doremir_list(int count, ...);
doremir_set_t doremir_set(int count, ...);
doremir_map_t doremir_map(int count, ...);

#define list0()                             doremir_list(0)
#define list1(a)                            doremir_list(1,a)
#define list2(a,b)                          doremir_list(2,a,b)
#define list3(a,b,c)                        doremir_list(3,a,b,c)
#define list4(a,b,c,d)                      doremir_list(4,a,b,c,d)
#define list5(a,b,c,d,e)                    doremir_list(5,a,b,c,d,e)
#define list(...) VARARG(list, __VA_ARGS__)

#define set0()                              doremir_set(0)
#define set1(a)                             doremir_set(1,a)
#define set2(a,b)                           doremir_set(2,a,b)
#define set3(a,b,c)                         doremir_set(3,a,b,c)
#define set4(a,b,c,d)                       doremir_set(4,a,b,c,d)
#define set5(a,b,c,d,e)                     doremir_set(5,a,b,c,d,e)
#define set(...) VARARG(set, __VA_ARGS__)

#define map0()                              doremir_map(0)
#define map1(a)                             doremir_map(1,a)
#define map2(a,b)                           doremir_map(2,a,b)
#define map3(a,b,c)                         doremir_map(3,a,b,c)
#define map4(a,b,c,d)                       doremir_map(4,a,b,c,d)
#define map5(a,b,c,d,e)                     doremir_map(5,a,b,c,d,e)
// #define map6(a,b,c,d,e,f)                   doremir_map(6,a,b,c,d,e,f)
#define map(...) VARARG(map, __VA_ARGS__)

#endif // _DOREMIR_UTIL_LITERALS

