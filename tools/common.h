
inline static 
fa_list_t just(fa_ptr_t x, fa_list_t xs)
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
                    fa_map_dget(fa_string("help"), OPTS)                  \
                    )                                                     \
                    fa_option_show_all(OPTIONS, (char *) ARGV[0]);        \
                    else                                                  \


#define fa_with_session_(S) \
    fa_with_temp(S, fa_audio_begin_session())

#define fa_with_default_out(S, O) \
    fa_let(O, fa_audio_default_output(S))

#define fa_open_stereo_out(ST, O, OUT) \
    fa_with_temp(ST, fa_audio_open_stream(NULL, O, just, OUT))  \
    if (fa_check(ST)) {                                         \
        fa_error_log(ST, NULL);                                 \
    } else
    

inline static
void fa_set_log_tool() {
#ifdef FAUDIO_DEBUG
    fa_set_log_std();
#else
    if(getenv("FAUDIO_DEBUG") && 0 != strcmp(getenv("FAUDIO_DEBUG"), "0")) {
        fa_set_log_std();        
    }
#endif
}

#define time_zero fa_milliseconds(0)
#define fa_action_compose(x,y) fa_action_many(list(pair(x, time_zero), pair(y, time_zero)))
#define fa_action_compose3(x,y,z) fa_action_many(list(pair(x, time_zero), pair(y, time_zero), pair(z, time_zero)))


inline static
fa_list_t just_list(fa_ptr_t x, fa_list_t xs)
{
    return x;
}
