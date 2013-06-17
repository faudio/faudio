
#ifndef _FAE_UTIL_MACROS
#define _FAE_UTIL_MACROS

/** Execute following statement with a binding in scope.

    Syntax:

        fae_let(var, expr)
            statement;

    Example:

        fae_let(x, 23)
        {
            printf("%d\n", 23)
        }
 */
#define fae_let(var, expr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
         __c; \
         __c = ((__typeof__(expr)*) 0) \
         )

/** Execute the following statement with a binding in scope, then
    evaluate the given cleanup expression.

    Syntax:

        fae_with(var, expr, final)
            statement;

    Example:

        fae_with(x, get_resource(), release_resource(x))
        {
            use_resource(x);
        }

 */
#define fae_with(var, expr, destr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
        __c; \
        __c = ((__typeof__(expr)*) 0), destr \
        )

/** Execute the following statement with a binding in scope, then destroy
    the value using fae_destroy.

    Syntax:

        fae_with(var, expr, final)
            statement;

    Example:

        fae_with(x, get_resource(), release_resource(x))
        {
            use_resource(x);
        }

 */
#define fae_with_temp(var, expr) \
    fae_with(var, expr, fae_destroy(var))
    
/** Execute the following statement once for each item in the
    given list.

    Syntax:

        fae_for_each(var, list)
            statement;

    Example:

        fae_dloop(
            ptr_t, x, list(1,2,3)
        )
        {
            use_resource(x);
        }

 */
#define fae_for_each(var, list) \
    fae_dfor_each(var, fae_list_copy(list))

#define fae_dfor_each(var, list) \
    fae_with (__j, list, fae_list_destroy(__j)) \
        for (; !fae_list_is_empty(__j); __j = fae_list_dtail(__j)) \
            fae_let (var, fae_list_head(__j))

#define fae_for_each_last(var, list, last) \
    fae_dfor_each_last(var, fae_list_copy(list), last)

#define fae_dfor_each_last(var, list, last) \
    fae_with (__j, list, fae_list_destroy(__j)) \
        for (; !fae_list_is_empty(__j); __j = fae_list_dtail(__j)) \
            fae_let (var, fae_list_head(__j)) \
                fae_let (last, fae_list_is_single(__j))

#define fae_set_for_each(x,set) \
    fae_for_each(x,fae_set_to_list(set))

#define fae_map_for_each(x,set) \
    fae_for_each(x,fae_map_to_list(set))

#define match switch
#define against(label) case label: return
#define default(x) default: return (x)
#define no_default() default: assert(false && "Missing label")




#define write_to(s, t) \
    s = fae_string_append(s, t)


#endif // _FAE_UTIL_MACROS

