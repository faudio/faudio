/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_STRING
#define _SCLAUDIO_STRING

typedef SclChar* SclString;

#ifdef __cplusplus
extern "C" {
#endif

/** 
    \ingroup sclaudio 
    \file 
  */

// SCLAUDIO_API SclString       scl_schars_to_string(signed char* val);
// 
// SCLAUDIO_API SclString       scl_uchars_to_string(unsigned char* val);
// 
// SCLAUDIO_API signed char*    scl_string_to_schars(SclString val);
// 
// SCLAUDIO_API unsigned char*  scl_string_to_uchars(SclString val);

// /*
//     TODO
//     These are needed for testing right now...
//  */
// 
// SCL_INLINE SCLAUDIO_API int scl_string_length(SclString str)
// {
//     SclChar* end = str;
//     while(*end != 0) end++;
//     return end - str;
// }
// 
// /**
//     Convert an SclString to a C-style string. Non-Ascii characters yield
//     undefined code points.
//  */         
// // FIXME this is C++ inline code!
// SCL_INLINE SCLAUDIO_API char* scl_string_to_c_string(SclString str)
// {
// #ifndef SCL_UNICODE
//     return str;
// #else 
// #ifdef __cplusplus
//     int length = scl_string_length(str);
//     char* cstr = new char[length + 1];
//     for (int i = 0; i < length; ++i)
//         cstr[i] = (char) str[i];
//     cstr[length] = 0;               
//     return cstr;
// #endif // __cplusplus
// #endif
// }


#ifdef __cplusplus
}
#endif

#endif
