/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIOX_DEFINES                                      
#define _SCLAUDIOX_DEFINES

#include <stdint.h>
#include "sclaudio/defines.h"
#include "sclaudiox/config.h"

#ifdef _WIN32
    #define SCL_WIN
    #define SCL_OS WIN
#endif
#ifdef __APPLE__
    #define SCL_OSX
    #define SCL_OS OSX
#endif
#ifndef SCL_OS
    #error "Unknown operating system."
#endif

#ifdef SCL_WIN
    #ifndef SCLAUDIO_API
        #define SCLAUDIO_API __declspec(dllexport)
    #endif
#else
    #ifndef SCLAUDIO_API
        #define SCLAUDIO_API
    #endif
#endif

#ifdef SCL_DEBUG
    #ifndef SCL_LOG
        #define SCL_LOG
    #endif
#endif

#endif