
#ifndef _FA_UTIL_MACROS
#define _FA_UTIL_MACROS

/** Execute following statement with a binding in scope.

    Syntax:

        fa_let(var, expr)
            statement;

    Example:

        fa_let(x, 23)
        {
            printf("%d\n", 23)
        }
 */
#define fa_let(var, expr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
         __c; \
         __c = ((__typeof__(expr)*) 0) \
         )

/** Execute the following statement with a binding in scope, then
    evaluate the given cleanup expression.

    Syntax:

        fa_with(var, expr, final)
            statement;

    Example:

        fa_with(x, get_resource(), release_resource(x))
        {
            use_resource(x);
        }

 */
#define fa_with(var, expr, destr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
        __c; \
        __c = ((__typeof__(expr)*) 0), destr \
        )

/** Execute the following statement with a binding in scope, then destroy
    the value using fa_destroy.

    Syntax:

        fa_with(var, expr, final)
            statement;

    Example:

        fa_with(x, get_resource(), release_resource(x))
        {
            use_resource(x);
        }

 */
#define fa_with_temp(var, expr) \
    fa_with(var, expr, fa_destroy(var))
    
/** Execute the following statement once for each item in the
    given list.

    Syntax:

        fa_for_each(var, list)
            statement;

    Example:

        fa_dloop(
            ptr_t, x, list(1,2,3)
        )
        {
            use_resource(x);
        }

 */
#define fa_for_each(var, list) \
    fa_dfor_each(var, fa_list_copy(list))

#define fa_dfor_each(var, list) \
    fa_with (__j, list, fa_list_destroy(__j)) \
        for (; !fa_list_is_empty(__j); __j = fa_list_dtail(__j)) \
            fa_let (var, fa_list_head(__j))

#define fa_for_each_last(var, list, last) \
    fa_dfor_each_last(var, fa_list_copy(list), last)

#define fa_dfor_each_last(var, list, last) \
    fa_with (__j, list, fa_list_destroy(__j)) \
        for (; !fa_list_is_empty(__j); __j = fa_list_dtail(__j)) \
            fa_let (var, fa_list_head(__j)) \
                fa_let (last, fa_list_is_single(__j))

#define fa_set_for_each(x,set) \
    fa_for_each(x,fa_set_to_list(set))

#define fa_map_for_each(x,set) \
    fa_for_each(x,fa_map_to_list(set))

#define match switch
#define against(label) case label: return
#define default(x) default: return (x)
#define no_default() default: assert(false && "Missing label")




#define write_to(s, t) \
    s = fa_string_append(s, t)


#endif // _FA_UTIL_MACROS

