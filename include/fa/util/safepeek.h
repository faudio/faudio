
#ifndef _FA_UTIL_SAFEPEEK
#define _FA_UTIL_SAFEPEEK

static inline double safe_peek_number_default(fa_ptr_t ptr, double def) {
    return ptr ? fa_peek_number(ptr) : def;
}

static inline int64_t safe_peek_i64_default(fa_ptr_t ptr, int64_t def) {
    return ptr ? (int64_t) fa_peek_number(ptr) : def;
}

static inline int32_t safe_peek_i32_default(fa_ptr_t ptr, int32_t def) {
    return ptr ? (int32_t) fa_peek_number(ptr) : def;
}

#define safe_peek_number(ptr) safe_peek_number_default(ptr, 0)
#define safe_peek_i64(ptr) safe_peek_i64_default(ptr, 0)
#define safe_peek_i32(ptr) safe_peek_i32_default(ptr, 0)

#endif // _FA_UTIL_SAFEPEEK
