
#ifndef _FA_UTIL_LITERALS
#define _FA_UTIL_LITERALS

#include <fa/list.h>
#include <fa/set.h>
#include <fa/map.h>
#include <fa/util/vararg.h>

fa_list_t fa_list(int count, ...);
fa_set_t fa_set(int count, ...);
fa_map_t fa_map(int count, ...);

#define list0()                             fa_list(0)
#define list1(a)                            fa_list(1,a)
#define list2(a,b)                          fa_list(2,a,b)
#define list3(a,b,c)                        fa_list(3,a,b,c)
#define list4(a,b,c,d)                      fa_list(4,a,b,c,d)
#define list5(a,b,c,d,e)                    fa_list(5,a,b,c,d,e)
#define list6(a,b,c,d,e,f)                  fa_list(6,a,b,c,d,e,f)
#define list(...) VARARG(list, __VA_ARGS__)

#define set0()                              fa_set(0)
#define set1(a)                             fa_set(1,a)
#define set2(a,b)                           fa_set(2,a,b)
#define set3(a,b,c)                         fa_set(3,a,b,c)
#define set4(a,b,c,d)                       fa_set(4,a,b,c,d)
#define set5(a,b,c,d,e)                     fa_set(5,a,b,c,d,e)
#define set(...) VARARG(set, __VA_ARGS__)

#define map0()                              fa_map(0)
#define map1(a)                             fa_map(1,a)
#define map2(a,b)                           fa_map(2,a,b)
#define map3(a,b,c)                         fa_map(3,a,b,c)
#define map4(a,b,c,d)                       fa_map(4,a,b,c,d)
#define map5(a,b,c,d,e)                     fa_map(5,a,b,c,d,e)
// #define map6(a,b,c,d,e,f)                   fa_map(6,a,b,c,d,e,f)
#define map(...) VARARG(map, __VA_ARGS__)

#define concat0()                           fa_string_empty()
#define concat1(a)                          a
#define concat2(a,b)                        fa_string_append(a,b)
#define concat3(a,b,c)                      concat2(a,concat2(b,c))
#define concat4(a,b,c,d)                    concat2(a,concat3(b,c,d))
#define concat5(a,b,c,d,e)                  concat2(a,concat4(b,c,d,e))
#define concat(...) VARARG(concat, __VA_ARGS__)

#define dconcat0()							fa_string_empty()
#define dconcat1(a)							a
#define dconcat2(a,b)                       fa_string_dappend(a,b)
#define dconcat3(a,b,c)                     dconcat2(a,dconcat2(b,c))
#define dconcat4(a,b,c,d)                   dconcat2(a,dconcat3(b,c,d))
#define dconcat5(a,b,c,d,e)                 dconcat2(a,dconcat4(b,c,d,e))
#define dconcat(...) VARARG(dconcat, __VA_ARGS__)

#define pair(a,b)                           fa_pair_create(a,b)
#define pair_left(a,b)                      fa_pair_left_create(a,b)


#endif // _FA_UTIL_LITERALS

