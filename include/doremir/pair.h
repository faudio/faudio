
#ifndef _DOREMIR_PAIR
#define _DOREMIR_PAIR

#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPair Pair
    @{
    */

typedef size_t doremir_pair_type_t;
typedef intptr_t doremir_pair_func_t;
typedef struct {
            intptr_t fst; intptr_t snd;
        } doremir_pair_t;
doremir_pair_t doremir_pair_map(doremir_pair_func_t,
                                doremir_pair_t);

/** @}
    @}
    */

#endif // _DOREMIR_PAIR

