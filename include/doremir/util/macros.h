
#ifndef _DOREMIR_UTIL_MACROS
#define _DOREMIR_UTIL_MACROS

/** Execute following statement with a binding in scope.

    Syntax:
        
        doremir_with(type, var, initExpr, finalizeExpr) 
            statement;
    
    Example:      
    
        doremir_let(
            int, x, 23
        )
        {
            printf("%d\n", 23)
        }
 */
#define doremir_let(type, var, bind) \
    for (type var = bind, *__c=((type*) 1); \
         __c; \
         __c = ((type*) 0) \
         )

/** Execute the following statement with a binding in scope, then
    evaluate the given finalizer expression.

    Syntax:

        doremir_with(type, var, initExpr, finalizeExpr) 
            statement;
    
    Example:      
    
        doremir_with(
            resource_t, x, get_resource(), 
            release_resource(x)
        )
        {
            use_resource(x);
        }
    
 */
#define doremir_with(type, var, bind, unbind) \
    for (type var = bind, *__c=((type*) 1); \
        __c; \
        __c = ((type*) 0), unbind \
        )

/** Execute the following statement once for each item in the
    given list.
    
    Syntax:
        
        doremir_loop(listExpr, lastVar, elemVar)
            statement;
    
    Example:      
    
        doremir_dloop(
            ptr_t, x, list(1,2,3)
        )
        {
            use_resource(x);
        }
    
 */
#define doremir_list_for_each(list, last, var) \
    doremir_list_dfor_each(doremir_list_copy(list), last, var)

#define doremir_list_dfor_each(list, last, var) \
    doremir_with ( \
        doremir_list_t, __j, list, \
        doremir_list_destroy(__j) \
        ) \
        for ( ; \
            !doremir_list_is_empty(__j); \
            __j = doremir_list_dtail(__j) \
            ) \
                doremir_let ( \
                    ptr_t, var, doremir_list_head(__j) \
                    ) \
                    doremir_let ( \
                        bool, last, doremir_list_is_single(__j) \
                        )

#endif // _DOREMIR_UTIL_MACROS

