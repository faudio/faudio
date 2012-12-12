
#ifndef _DOREMIR_PAIR
#define _DOREMIR_PAIR

#include <doremir.h>
#include <doremir/std.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirPair Pair
    @{
    */

typedef size_t doremir_pair_type_t;
typedef doremir_ptr_t doremir_pair_unary_t;
typedef struct {
            doremir_ptr_t fst; doremir_ptr_t snd;
        } doremir_pair_t;
doremir_pair_t doremir_pair_map(doremir_pair_unary_t,
                                doremir_pair_t);

/** @}
    @}
    */

#endif // _DOREMIR_PAIR

