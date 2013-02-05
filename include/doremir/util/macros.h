
#ifndef _DOREMIR_UTIL_MACROS
#define _DOREMIR_UTIL_MACROS

/** Execute following statement with a binding in scope.

    Syntax:

        doremir_let(var, expr)
            statement;

    Example:

        doremir_let(x, 23)
        {
            printf("%d\n", 23)
        }
 */
#define doremir_let(var, expr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
         __c; \
         __c = ((__typeof__(expr)*) 0) \
         )

/** Execute the following statement with a binding in scope, then
    evaluate the given cleanup expression.

    Syntax:

        doremir_with(var, expr, final)
            statement;

    Example:

        doremir_with(x, get_resource(), release_resource(x))
        {
            use_resource(x);
        }

 */
#define doremir_with(var, expr, destr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
        __c; \
        __c = ((__typeof__(expr)*) 0), destr \
        )

/** Execute the following statement once for each item in the
    given list.

    Syntax:

        doremir_for_each(var, list)
            statement;

    Example:

        doremir_dloop(
            ptr_t, x, list(1,2,3)
        )
        {
            use_resource(x);
        }

 */
#define doremir_for_each(var, list) \
    doremir_dfor_each(var, doremir_list_copy(list))

#define doremir_dfor_each(var, list) \
    doremir_with (__j, list, doremir_list_destroy(__j)) \
        for (; !doremir_list_is_empty(__j); __j = doremir_list_dtail(__j)) \
            doremir_let (var, doremir_list_head(__j))

#define doremir_for_each_last(var, list, last) \
    doremir_dfor_each_last(var, doremir_list_copy(list), last)

#define doremir_dfor_each_last(var, list, last) \
    doremir_with (__j, list, doremir_list_destroy(__j)) \
        for (; !doremir_list_is_empty(__j); __j = doremir_list_dtail(__j)) \
            doremir_let (var, doremir_list_head(__j)) \
                doremir_let (last, doremir_list_is_single(__j))

#define doremir_set_for_each(x,set) \
    doremir_for_each(x,doremir_set_to_list(set))

#define doremir_map_for_each(x,set) \
    doremir_for_each(x,doremir_map_to_list(set))

#define match switch
#define against(label) case label: return
#define default(x) default: return (x)
#define no_default() default: assert(false && "Missing label")




#define write_to(s, t) \
    s = doremir_string_append(s, t)


#endif // _DOREMIR_UTIL_MACROS

