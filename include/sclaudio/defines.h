/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */
 
#ifndef _SCLAUDIO_DEFINES
#define _SCLAUDIO_DEFINES

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

#endif
