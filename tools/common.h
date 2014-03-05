
inline static 
list_t just(ptr_t x, list_t xs)
{
    return x;
}

// TODO move
#define fa_map_get_the(TYPE, KEY, MAP) \
    (fa_peek_##TYPE(fa_map_get(KEY, MAP)))

#define fa_map_get_int32(KEY, MAP) fa_map_get_the(int32, KEY, MAP)
#define fa_map_get_int64(KEY, MAP) fa_map_get_the(int64, KEY, MAP)
#define fa_map_get_double(KEY, MAP) fa_map_get_the(double, KEY, MAP)

#define fa_map_get_or(TYPE, KEY, DEFAULT, MAP) \
    (fa_map_get(KEY, opts) ? fa_peek_##TYPE(fa_map_get(KEY, MAP)) : DEFAULT)

#define fa_map_get_int32_or(KEY, DEFAULT, MAP) fa_map_get_or(int32, KEY, DEFAULT, MAP)
#define fa_map_get_int64_or(KEY, DEFAULT, MAP) fa_map_get_or(int64, KEY, DEFAULT, MAP)
#define fa_map_get_double_or(KEY, DEFAULT, MAP) fa_map_get_or(double, KEY, DEFAULT, MAP)

#define fa_sizeof_array(A) sizeof(A) / sizeof(A[0])
#define fa_option_show_all(A,S) fa_option_show(fa_sizeof_array(A),A,S)
#define fa_option_parse_all(A,AC,AV) fa_option_parse(fa_sizeof_array(A), A, AC, AV)

inline static
int fa_initialize2()
{
    fa_initialize();
    return 0;
}

inline static
void fa_terminate2(int x)
{
    fa_terminate();
}


#define fa_with_faudio() \
    fa_with(__faudio__, fa_initialize2(), fa_terminate2(__faudio__))

#define fa_with_options(OPTIONS, ARGC, ARGV, OPTS, ARGS) \
    fa_let(__res__, fa_option_parse_all(OPTIONS, ARGC, (char **) ARGV))   \
        fa_let(OPTS, fa_pair_first(__res__))                              \
            fa_let(ARGS, fa_pair_second(__res__))                         \
                if (                                                      \
                    (OPTS != (ARGS + 1)) && /* Use variables */           \
                    fa_map_has_key(string("h"), OPTS)                     \
                    )                                                     \
                    fa_option_show_all(OPTIONS, (char *) ARGV[0]);        \
                    else                                                  \

