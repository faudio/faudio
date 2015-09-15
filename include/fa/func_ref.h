
#ifndef _FA_FUNC_REF
#define _FA_FUNC_REF

#include <fa.h>
#include <fa/list.h>

/** @addtogroup FaFuncRef
 
    Function reference

    @par Implements 
    - fa_destroy_t
    - fa_string_show_t

    @defgroup Fa Fa
    @{
    @defgroup FaFuncRef FuncRef
    @{
    */

/** The abstract type of function references.
*/
typedef struct _fa_func_ref_t * fa_func_ref_t;

/** Create a new function reference.
*/
fa_func_ref_t fa_func_ref_create(void* func, fa_ptr_t data);

/** Destroy the given function reference. The data is not affected.
*/
void fa_func_ref_destroy(fa_func_ref_t func_ref);

/** Destroy the given function reference. The data is not affected.
*/
void fa_func_ref_deep_destroy(fa_func_ref_t func_ref, fa_deep_destroy_pred_t pred);

/** Get the function pointer of the given function reference.
*/
void* fa_func_ref_func(fa_func_ref_t func_ref);

/** Get the data component of the given function reference.
*/
fa_ptr_t fa_func_ref_data(fa_func_ref_t func_ref);


void fa_func_ref_log_count();

/** @}
    @}
    */

#endif // _FA_FUNC_REF

