/*
    ScoreCleaner Audio Engine

    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _TEST
#define _TEST

/**
    @file test.h
    Super simple test framework.
 */

typedef enum SclTestFlag
{
    SclTestNoFlag       = 0,
    SclTestVerbose      = 1
};

/**
    Last result of test.
    Zero or greater indicate index of failed child test.
 */
typedef enum SclTestResult
{
    SclTestPredicateFailed  = -2,
    SclTestAllSucceeded     = -1
};

struct SclTest;

typedef void (*SclTestBefore)    (SclTest* test);
typedef int  (*SclTestPredicate) (SclTest* test);
typedef void (*SclTestAfter)     (SclTest* test);

struct SclTest
{
    /** 
        Name of test 
     */
    char *name;

    /** 
        Parent test or NULL. 
     */
    void *parent;

    /** 
        Routine to run before tests or NULL. 
     */
    SclTestBefore before;

    /** 
        Test function or NULL. 
     */
    SclTestPredicate predicate;

    /** 
        Child tests if any. 
     */
    int numberOfChildren;
    SclTest **children;

    /** 
        Routine to run after tests or null. 
     */
    SclTestAfter after;

    /** 
        Result of last run. 
     */
    SclTestResult status;
    
    /**
        Current level.
     */
    int level;
};


SclTest* scl_new_test(char*             name,
                      SclTest*          parent,
                      SclTestBefore     before,
                      SclTestPredicate  predicate,
                      SclTestAfter      after);

/**
    Free the given test.
 */
void scl_free_test(SclTest* test);

/**
    Free the given test and all its descendants.
 */
void scl_free_test_deep(SclTest* test);

/**
    Print children of the given test.
 */
void scl_print_children(SclTest* test);

/**
    Returns the full name of a test, including the name
    of its parents, and separated by '/' characters.
 */
char* scl_test_get_full_name(SclTest* test);

/**
    Runs the given test and its descendants.
    Returns index of the first failed child, or SclTestPredicateFailed or SclTestAllSucceeded.
 */
SclTestResult scl_run_test(SclTest* test, int verbose);
    
/**
    Print all tests descendant from the given test which were run and failed during the
    last call to scl_run_test().
 */
void scl_print_failed_tests(SclTest* test);

/**
    Reset the failure status of the given test and its descendants.
    (This will set the failed property to SclTestAllSucceeded).
 */
void scl_reset_test(SclTest* test);

/**
    Print if possible.
 */
void scl_test_print(SclTest* test, char* format, ...);


/**
    Macro for defining a simple test with a predicate.
 */
#define SCL_TEST(parent, name, pred) \
    SclTest* name = scl_new_test((char*)#name, parent, NULL, pred, NULL);

/**
    Macro for defining a simple test with a standard predicate (test_name_p).
 */
#define SCL_TEST_STD(parent, name) \
    SclTest* name = scl_new_test((char*)#name, parent, NULL, name##_p, NULL);

/**
    Macro for defining a suite with setup.
 */
#define SCL_TEST_SUITE(parent, name, before, after) \
    SclTest* name = scl_new_test((char*)#name, parent, before, NULL, after);

/**
    Macro for defining a suite with standard setup (suite_name_before and suite_name_after).
 */
#define SCL_TEST_SUITE_STD(parent, name) \
    SclTest* name = scl_new_test((char*)#name, parent, name##_before, NULL, name##_after);

/**
    Macro for defining a test with setup.
 */
#define SCL_TEST_WITH_SETUP(parent, name, before, pred, after) \
    SclTest* name = scl_new_test((char*)#name, parent, before, pred, after);



/**
    Basic main function that runs the test returned by the given function and prints
    the result. 
 */
#define SCL_TEST_MAIN(init)                                         \
    int main(int argc, char const *argv[])                          \
    {                                                               \
        printf("\n");                                               \
        SclTest *all = init();                                      \
        SclTestResult result = scl_run_test(all, 2);                \
                                                                    \
        printf("\n");                                               \
        if (result == SclTestAllSucceeded)                          \
        {                                                           \
            printf("All tests passed\n");                           \
        }                                                           \
        else                                                        \
        {                                                           \
            printf("Some tests failed:\n");                         \
            scl_print_failed_tests(all);                            \
        }                                                           \
    	scl_free_test_deep(all);                                    \
                                                                    \
        printf("\n");                                               \
        return result - SclTestAllSucceeded;                        \
    }

#endif