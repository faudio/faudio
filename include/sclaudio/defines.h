/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_DEFINES
#define _SCLAUDIO_DEFINES

#include <stdint.h>
#include "sclaudiox/config.h"

#ifdef _WIN32
    #ifndef SCLAUDIO_API
        #define SCLAUDIO_API __declspec(dllexport)
    #endif
#else
    #ifndef SCLAUDIO_API
        #define SCLAUDIO_API
    #endif
#endif

#ifdef SCL_LISPWORKS
    #define SCL_INLINE
#else
    #define SCL_INLINE inline
#endif

#ifdef SCL_UNICODE
    typedef uint16_t  SclChar;
#else
    typedef char      SclChar;
#endif
#ifdef Float32
    #define float Float32
#endif
#ifdef Float64
    #define double Float64
#endif

#endif
