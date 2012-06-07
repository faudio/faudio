/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_TEST
#define _SCLAUDIO_TEST

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

SCLAUDIO_API void            scl_test_nothing();
SCLAUDIO_API void            scl_test_error(int variant, SclError* err);                                       
SCLAUDIO_API void            scl_test_two_errors(int variant, SclPortmidiError *merr, SclPortaudioError *aerr);                                       
  
SCLAUDIO_API int             scl_test_pass_int(int x);
SCLAUDIO_API float           scl_test_pass_float(float x);
SCLAUDIO_API double          scl_test_pass_double(double x);
SCLAUDIO_API SclTimeUnit     scl_test_pass_enum(SclTimeUnit x);
SCLAUDIO_API SclString       scl_test_pass_string(SclString x);
SCLAUDIO_API SclAtom         scl_test_pass_atom(SclAtom x);
SCLAUDIO_API void*           scl_test_pass_object(SclAudioDevice obj);

SCLAUDIO_API void            scl_test_pass_list_int(int *x, int len);
SCLAUDIO_API void            scl_test_pass_list_string(SclString *x, int len);
SCLAUDIO_API void            scl_test_pass_list_object(void **x, int len);
SCLAUDIO_API void            scl_test_pass_list_atom(SclAtom *x, int len);  
SCLAUDIO_API void            scl_test_pass_list_list_int(int **x, int *len1, int len2);
  
SCLAUDIO_API int             scl_test_return_int();
SCLAUDIO_API float           scl_test_return_float();
SCLAUDIO_API double          scl_test_return_double();
SCLAUDIO_API SclTimeUnit     scl_test_return_enum();
SCLAUDIO_API SclString       scl_test_return_string();
SCLAUDIO_API SclAtom         scl_test_return_atom();
SCLAUDIO_API void*           scl_test_return_object();

SCLAUDIO_API int*            scl_test_return_list_int(int *len);
SCLAUDIO_API SclString*      scl_test_return_list_string(int *len);
SCLAUDIO_API void**          scl_test_return_list_object(int *len);  
SCLAUDIO_API SclAtom*        scl_test_return_list_atom(int *len);  
SCLAUDIO_API int**           scl_test_return_list_list_int(int **len1 , int *len2); 

#ifdef __cplusplus
}
#endif

#endif
