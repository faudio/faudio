
#ifndef _DOREMIR_UTIL_VARARG
#define _DOREMIR_UTIL_VARARG

#define VA_NARGS_IMPL(_1, _2, _3, _4, _5, _6, N, ...) N
#define VA_NARGS(...) VA_NARGS_IMPL(X,##__VA_ARGS__, 5, 4, 3, 2, 1, 0)
#define VARARG_IMPL2(base, count, ...) base##count(__VA_ARGS__)
#define VARARG_IMPL(base, count, ...) VARARG_IMPL2(base, count, __VA_ARGS__)
#define VARARG(base, ...) VARARG_IMPL(base, VA_NARGS(__VA_ARGS__), __VA_ARGS__)

// #define VA_NARGS_IMPL(_1, _2, _3, _4, _5, _6, _7, _8, _9, N, ...) N
// #define VA_NARGS(...) VA_NARGS_IMPL(X,##__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
// #define VARARG_IMPL2(base, count, ...) base##count(__VA_ARGS__)
// #define VARARG_IMPL(base, count, ...) VARARG_IMPL2(base, count, __VA_ARGS__)
// #define VARARG(base, ...) VARARG_IMPL(base, VA_NARGS(__VA_ARGS__), __VA_ARGS__)


#endif // _DOREMIR_UTIL_VARARG

