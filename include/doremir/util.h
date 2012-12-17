
#ifndef _DOREMIR_UTIL
#define _DOREMIR_UTIL

#include <doremir.h>
#include <doremir/list.h> // for list forward decl

doremir_list_t doremir_list(int count, ...);

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

#define list_t doremir_list_t
#define list doremir_list
#define empty doremir_list_empty
#define cons doremir_list_cons
#define snoc doremir_list_snoc
#define destroy doremir_list_destroy
#define copy doremir_list_copy
#define swap doremir_list_swap
#define is_empty doremir_list_is_empty
#define lenght doremir_list_lenght
#define head doremir_list_head
#define tail doremir_list_tail
#define init doremir_list_init
#define last doremir_list_last
#define take doremir_list_take
#define drop doremir_list_drop
#define elem doremir_list_is_elem
#define reverse doremir_list_reverse
#define sort doremir_list_sort
#define filter doremir_list_filter
// #define map doremir_list_map
// #define foldl doremir_list_foldl
#define consd doremir_list_consd
// #define snocd doremir_list_snocd
// #define reversed doremir_list_reversed
// #define sortd doremir_list_sortd
// #define mapd doremir_list_mapd
#define sum doremir_list_sum

#define string_t doremir_string_t
#define string   doremir_string_from_utf8
#define unstring doremir_string_to_utf8
#define slength  doremir_string_length
#define sappend  doremir_string_append
#define sdappend doremir_string_dappend
#define char_at  doremir_string_char_at


#endif // _DOREMIR_UTIL

