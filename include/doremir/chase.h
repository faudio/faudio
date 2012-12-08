
#ifndef _DOREMIR_CHASE
#define _DOREMIR_CHASE



/** @defgroup Doremir
    @{
    @defgroup Chase
    @{
    */

typedef intptr_t doremir_chase_t;
typedef intptr_t doremir_chase_value_t;
doremir_chase_doremir_chase_chase_t doremir_chase_empty();
doremir_chase_doremir_chase_chase_t doremir_chase_create(doremir_chase_type_t,
                                                         size_t);
void doremir_chase_destroy(doremir_chase_doremir_chase_chase_t);
void doremir_chase_append(doremir_chase_doremir_chase_chase_t,
                          doremir_chase_doremir_chase_chase_t);
bool doremir_chase_is_empty(doremir_chase_doremir_chase_chase_t);
bool doremir_chase_is_single(doremir_chase_doremir_chase_chase_t);
int doremir_chase_lenght(doremir_chase_doremir_chase_chase_t);
typedef intptr_t doremir_chase_unary_func_t;
doremir_chase_doremir_chase_chase_t doremir_chase_map(doremir_chase_doremir_chase_unary_func_t,
                                                      doremir_chase_doremir_chase_chase_t);
doremir_chase_doremir_chase_chase_t doremir_chase_map_dest(doremir_chase_doremir_chase_unary_func_t,
                                                           doremir_chase_doremir_chase_chase_t);

/** @}
    @}
    */

#endif // _DOREMIR_CHASE

