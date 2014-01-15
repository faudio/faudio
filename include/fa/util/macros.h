
#ifndef _FA_UTIL_MACROS
#define _FA_UTIL_MACROS

/** Execute following statement with a binding in scope.

    ### Syntax

    ~~~
    fa_let(var, expr)
        statement;
    ~~~

    ### Example

    ~~~
    fa_let(x, 23)
    {
        printf("%d\n", 23)
    }
    ~~~
 */
#define fa_let(var, expr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
         __c; \
         __c = ((__typeof__(expr)*) 0) \
         )

/** Execute the following statement with a binding in scope, then
    evaluate the given finalizer expression.

    You can escape with-context with either continue or break: note that
    continue *will* execute the finalizer expression, break will *not*.

    ### Syntax

    ~~~
    fa_with(var, expr, final_expr)
        statement;
    ~~~

    ### Example
    
    ~~~
    fa_with(x, get_resource(), release_resource(x))
    {
        use_resource(x);
    }
    ~~~
 */
#define fa_with(var, expr, destr) \
    for (__typeof__(expr) var = expr, *__c=((__typeof__(expr)*) 1); \
        __c; \
        __c = ((__typeof__(expr)*) 0), destr \
        )

/** The same as fa_with, but uses fa_destroy as the finalize expression.

    You can escape with-context with either continue or break: note that
    continue *will* execute the finalizer expression, break will *not*.

    ### Syntax

    ~~~
    fa_with(var, expr)
        statement;
    ~~~

    ### Example

    ~~~
    fa_with(x, get_resource())
    {
        use_resource(x);
    }
    ~~~
 */
#define fa_with_temp(var, expr) \
    fa_with(var, expr, fa_destroy(var))
    
/** Execute the following statement once for each item in the
    given list.

    ### Syntax

    ~~~
    fa_for_each(var, list)
        statement;
    ~~~

    ### Example

    ~~~
    fa_for_each(x, list(1,2,3))
    {
        use_resource(x);
    }
    ~~~
 */
#define fa_for_each(var, list) \
    fa_dfor_each(var, fa_list_copy(list))

/** Execute the following statement once for each item in the
    given list (destructive).

    ### Syntax

    ~~~
    fa_dfor_each(var, list)
        statement;
    ~~~

    ### Example

    ~~~
    fa_dfor_each(x, list(1,2,3))
    {
        use_resource(x);
    }
    ~~~
 */
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

#define fa_push_list(x, xs) \
    xs = fa_list_cons(x, xs)

#define fa_dpush_list(x, xs) \
    xs = fa_list_dcons(x, xs)

#define fa_write_string(s, t) \
    s = fa_string_append(s, t)

/** Execute following statement while locking on the given mutex.

    You can escape locking context with either continue or break: note that
    continue *will* release the lock, break will *not*.

    ### Syntax

    ~~~
    fa_with_lock(mutex)
        statement;
    ~~~
    
    ### Example

    ~~~
    fa_with_lock(myMutex)
    {
        if (error_condition()) {
            continue;
        }
        do_syncronized_stuff();
    }
    ~~~
 */
#define fa_with_lock(mutex) \
    fa_with(__mutex, \
        fa_thread_lock(mutex), \
        fa_thread_unlock((void*) mutex + 0*((intptr_t)__mutex)))
    
/** @}
    @}
    */

#endif // _FA_UTIL_MACROS

