/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */ 

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cstdarg>

#include "test.h"

#define SCL_TEST_MAX_CHILDREN 50

void scl_add_child(SclTest* parent, 
                   SclTest* child)
{                   
    if (!parent) return;
    if (parent->numberOfChildren >= SCL_TEST_MAX_CHILDREN)
    {
        printf("Error: Too many children\n");
        exit(-1);
    }
    parent->children[parent->numberOfChildren++] = child;
}      

void scl_print_children(SclTest* test)
{
    for (int child = 0; child < test->numberOfChildren; ++child)
        printf("%s\n", test->children[child]->name);
}

SclTest* scl_new_test(char*             name, 
                      SclTest*          parent,
                      SclTestBefore     before,
                      SclTestPredicate  predicate,
                      SclTestAfter      after)
{
    SclTest* test     = new SclTest;

    test->name             = name;
    test->parent           = parent;
    test->before           = before;
    test->predicate        = predicate;
    test->after            = after;
    test->status           = SclTestAllSucceeded;
    test->numberOfChildren = 0;
    test->children         = new SclTest*[SCL_TEST_MAX_CHILDREN];
    test->level            = 0;

    scl_add_child(parent, test);
    return test;
}

void scl_free_test(SclTest* test)
{             
    delete test->children;
    delete test;
}

void scl_free_test_deep(SclTest* test)
{
    for (int i = 0; i < test->numberOfChildren; ++i)
        scl_free_test_deep(test->children[i]);        
    scl_free_test(test);
}

char* scl_test_get_full_name(SclTest* test)
{                            
    if (!test) return NULL;

    int   len = 0;
    char* buffer = (char*)"";

    while (test != NULL)
    {        
        int len2 = strlen(test->name);
        len += len2 + 2;

        char* prev = buffer;
        buffer = new char[len];
        
        strcpy(buffer, test->name);
        strcpy(buffer + len2, "/");
        strcpy(buffer + len2 + 1, prev);

        test = (SclTest*) test->parent;
    }                       
    buffer[len-1] = 0; // TODO leaks
    return buffer;
}


void scl_test_indent(int level)
{
    for (int i = 0; i < level; ++i)
        printf("  ");
}

void scl_test_horizontal_line(int level)
{               
    for (int i = 0; i < 50; ++i)
        printf("%s", "=");
    printf("\n");
}

void scl_test_print(SclTest* test, char* format, ...)
{
    scl_test_indent(test->level);
    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
    printf("\n");
}

SclTestResult scl_run_test_internal(SclTest* test, int verbose, int level)
{             
    // Prepare

    test->level = level;
    
    if (verbose >= 1 && level < 4)
    {
        scl_test_indent(level);
        scl_test_horizontal_line(level);
    }
    if (verbose >= 3)
    {
        scl_test_indent(level);
        printf("Preparing %s\n", test->name);
    }

    if (test->before)       
        test->before(test);
    

    // Run tests

    if (verbose >= 1)
    {
        scl_test_indent(level);
        printf("Running %s\n", test->name);
    }

    if (test->predicate)
    {
        if (!test->predicate(test))
        {
            test->status = SclTestPredicateFailed;
            goto finish;
        }
    }

    for (int child = 0; child < test->numberOfChildren; ++child)
    {
        if ( scl_run_test_internal(test->children[child], verbose, level + 1)
                  != 
             SclTestAllSucceeded )
        {
            test->status = (SclTestResult) child;
            // Continue running, only predicate cancels
        }
    }
    
    
finish:
    // Clean up

    if (verbose >= 2)
    {
        scl_test_indent(level);
        printf("%s %s\n", test->status == SclTestAllSucceeded ? "Passed" : "Failures in", test->name);
    }
    
    if (verbose >= 3)
    {
        scl_test_indent(level);
        printf("Cleaning up %s\n", test->name);
    }

    if (test->after)
        test->after(test);

    return test->status;
}

SclTestResult scl_run_test(SclTest* test, int verbose)
{
    return scl_run_test_internal(test, verbose, 0);
}
      

void scl_print_failed_tests(SclTest* test)
{
    if (test->status != SclTestAllSucceeded)
    {
        printf("   %s\n", scl_test_get_full_name(test));
    }
    for (int child = 0; child < test->numberOfChildren; ++child)
        scl_print_failed_tests(test->children[child]);
}

void scl_reset_test(SclTest* test)
{
    test->status = SclTestAllSucceeded;
    for (int child = 0; child < test->numberOfChildren; ++child)
        scl_reset_test(test->children[child]);
}
   

