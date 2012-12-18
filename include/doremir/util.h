
#ifndef _DOREMIR_UTIL
#define _DOREMIR_UTIL

#include <doremir.h>
#include <doremir/list.h> // for list forward decl

doremir_list_t doremir_list(int count, ...);

#define ptr_t  doremir_ptr_t
#define pair_t doremir_pair_t
#define list_t doremir_list_t
#define string_t doremir_string_t

#define tb      doremir_to_bool
#define ti8     doremir_to_int8
#define ti16    doremir_to_int16
#define ti32    doremir_to_int32
#define ti64    doremir_to_int64
#define td      doremir_to_double
#define b       doremir_from_bool
#define i8      doremir_from_int8
#define i16     doremir_from_int16
#define i32     doremir_from_int32
#define i64     doremir_from_int64
#define d       doremir_from_double

#define eq      doremir_equal
#define gt      doremir_greater_than
#define lt      doremir_less_than

#define tbool   doremir_to_bool
#define tint8   doremir_to_int8
#define tint16  doremir_to_int16
#define tint32  doremir_to_int32
#define tint64  doremir_to_int64
#define tfloat  doremir_to_float
#define tdouble doremir_to_double
#define fbool   doremir_from_bool
#define fint8   doremir_from_int8
#define fint16  doremir_from_int16
#define fint32  doremir_from_int32
#define fint64  doremir_from_int64
#define ffloat  doremir_from_float
#define fdouble doremir_from_double

#define pair(a,b)           doremir_pair_create(a,b)

#define VA_NARGS_IMPL(_1, _2, _3, _4, _5, _6, N, ...) N
#define VA_NARGS(...) VA_NARGS_IMPL(X,##__VA_ARGS__, 5, 4, 3, 2, 1, 0)
#define VARARG_IMPL2(base, count, ...) base##count(__VA_ARGS__)
#define VARARG_IMPL(base, count, ...) VARARG_IMPL2(base, count, __VA_ARGS__) 
#define VARARG(base, ...) VARARG_IMPL(base, VA_NARGS(__VA_ARGS__), __VA_ARGS__)

#define list0()             doremir_list(0)
#define list1(a)            doremir_list(1,a)
#define list2(a,b)          doremir_list(2,a,b)
#define list3(a,b,c)        doremir_list(3,a,b,c)
#define list4(a,b,c,d)      doremir_list(4,a,b,c,d)
#define list5(a,b,c,d,e)    doremir_list(5,a,b,c,d,e)
#define list(...) VARARG(list, __VA_ARGS__)

#define empty doremir_list_empty
#define cons doremir_list_cons
#define snoc doremir_list_snoc
#define append  doremir_list_append
#define destroy doremir_list_destroy
#define copy doremir_list_copy
#define swap doremir_list_swap
#define is_empty doremir_list_is_empty
#define is_single doremir_list_is_single
#define lenght doremir_list_lenght
#define head doremir_list_head
#define tail doremir_list_tail
#define init doremir_list_init
#define last doremir_list_last
#define take doremir_list_take
#define drop doremir_list_drop
#define has_elem doremir_list_has_elem
#define reverse doremir_list_reverse
#define sort doremir_list_sort
#define filter doremir_list_filter
#define consd doremir_list_consd
#define snocd doremir_list_snocd
#define reversed doremir_list_reversed
#define sortd doremir_list_sortd
#define mapd doremir_list_mapd
#define sum doremir_list_sum

#define string   doremir_string_from_utf8
#define unstring doremir_string_to_utf8
#define slength  doremir_string_length
#define sappend  doremir_string_append
#define sdappend doremir_string_dappend
#define char_at  doremir_string_char_at
#define sshow    doremir_string_show


#endif // _DOREMIR_UTIL

